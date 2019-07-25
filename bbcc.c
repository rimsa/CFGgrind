/*--------------------------------------------------------------------*/
/*--- Callgrind                                                    ---*/
/*---                                                       bbcc.c ---*/
/*--------------------------------------------------------------------*/

/*
 This file is part of Callgrind, a Valgrind tool for call tracing.

 Copyright (C) 2002-2017, Josef Weidendorfer (Josef.Weidendorfer@gmx.de)

 This program is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License as
 published by the Free Software Foundation; either version 2 of the
 License, or (at your option) any later version.

 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 02111-1307, USA.

 The GNU General Public License is contained in the file COPYING.
 */

#include "global.h"

#include "pub_tool_threadstate.h"

/*------------------------------------------------------------*/
/*--- BBCC operations                                      ---*/
/*------------------------------------------------------------*/

#define N_BBCC_INITIAL_ENTRIES  10437

/* BBCC table (key is BB/Context), per thread, resizable */
bbcc_hash current_bbccs;

void LPG_(init_bbcc_hash)(bbcc_hash* bbccs) {
	Int i;

	LPG_ASSERT(bbccs != 0);

	bbccs->size = N_BBCC_INITIAL_ENTRIES;
	bbccs->entries = 0;
	bbccs->table = (BBCC**) LPG_MALLOC("cl.bbcc.ibh.1",
			bbccs->size * sizeof(BBCC*));

	for (i = 0; i < bbccs->size; i++)
		bbccs->table[i] = NULL;
}

void LPG_(copy_current_bbcc_hash)(bbcc_hash* dst) {
	LPG_ASSERT(dst != 0);

	dst->size = current_bbccs.size;
	dst->entries = current_bbccs.entries;
	dst->table = current_bbccs.table;
}

bbcc_hash* LPG_(get_current_bbcc_hash)() {
	return &current_bbccs;
}

void LPG_(set_current_bbcc_hash)(bbcc_hash* h) {
	LPG_ASSERT(h != 0);

	current_bbccs.size = h->size;
	current_bbccs.entries = h->entries;
	current_bbccs.table = h->table;
}

void LPG_(forall_bbccs)(void (*func)(BBCC*)) {
	BBCC *bbcc;
	int i;

	for (i = 0; i < current_bbccs.size; i++) {
		if ((bbcc = current_bbccs.table[i]) == NULL)
			continue;

		while (bbcc) {
			(*func)(bbcc);
			bbcc = bbcc->next;
		}
	}
}

/* All BBCCs for recursion level 0 are inserted into a
 * thread specific hash table with key
 * - address of BB structure (unique, as never freed)
 * - current context (includes caller chain)
 * BBCCs for other recursion levels are in bbcc->rec_array.
 *
 * The hash is used in setup_bb(), i.e. to find the cost
 * counters to be changed in the execution of a BB.
 */

static __inline__ UInt bbcc_hash_idx(BB* bb, Context* cxt, UInt size) {
	LPG_ASSERT(bb != 0);
	LPG_ASSERT(cxt != 0);

	return ((Addr) bb + (Addr) cxt) % size;
}

/* Lookup for a BBCC in hash.
 */
static BBCC* lookup_bbcc(BB* bb, Context* cxt) {
	BBCC* bbcc = bb->last_bbcc;
	UInt idx;

	/* check LRU */
	if (bbcc->cxt == cxt) {
		if (bbcc->tid == LPG_(current_tid))
			return bbcc;
	}

	LPG_(stat).bbcc_lru_misses++;

	idx = bbcc_hash_idx(bb, cxt, current_bbccs.size);
	bbcc = current_bbccs.table[idx];
	while (bbcc && (bb != bbcc->bb || cxt != bbcc->cxt)) {
		bbcc = bbcc->next;
	}

	LPG_DEBUG(2, "  lookup_bbcc(BB %#lx, Cxt %u, fn '%s'): %p (tid %u)\n",
			bb_addr(bb), cxt->base_number, cxt->fn[0]->name, bbcc,
			bbcc ? bbcc->tid : 0);

	LPG_DEBUGIF(2)
		if (bbcc)
			LPG_(print_bbcc)(-2, bbcc);

	return bbcc;
}

/* double size of hash table 1 (addr->BBCC) */
static void resize_bbcc_hash(void) {
	Int i, new_size, conflicts1 = 0, conflicts2 = 0;
	BBCC** new_table;
	UInt new_idx;
	BBCC *curr_BBCC, *next_BBCC;

	new_size = 2 * current_bbccs.size + 3;
	new_table = (BBCC**) LPG_MALLOC("cl.bbcc.rbh.1", new_size * sizeof(BBCC*));

	for (i = 0; i < new_size; i++)
		new_table[i] = NULL;

	for (i = 0; i < current_bbccs.size; i++) {
		if (current_bbccs.table[i] == NULL)
			continue;

		curr_BBCC = current_bbccs.table[i];
		while (NULL != curr_BBCC) {
			next_BBCC = curr_BBCC->next;

			new_idx = bbcc_hash_idx(curr_BBCC->bb, curr_BBCC->cxt, new_size);

			curr_BBCC->next = new_table[new_idx];
			new_table[new_idx] = curr_BBCC;
			if (curr_BBCC->next) {
				conflicts1++;
				if (curr_BBCC->next->next)
					conflicts2++;
			}

			curr_BBCC = next_BBCC;
		}
	}

	LPG_FREE(current_bbccs.table);

	LPG_DEBUG(0, "Resize BBCC Hash: %u => %d (entries %u, conflicts %d/%d)\n",
			current_bbccs.size, new_size, current_bbccs.entries, conflicts1,
			conflicts2);

	current_bbccs.size = new_size;
	current_bbccs.table = new_table;
	LPG_(stat).bbcc_hash_resizes++;
}

/*
 * Allocate a new BBCC
 *
 * Uninitialized:
 * cxt, rec_index, rec_array, next_bbcc, next1, next2
 */
static __inline__ BBCC* new_bbcc(BB* bb) {
	BBCC* bbcc;

	/* We need cjmp_count+1 JmpData structs:
	 * the last is for the unconditional jump/call/ret at end of BB
	 */
	bbcc = (BBCC*) LPG_MALLOC("cl.bbcc.nb.1",
			sizeof(BBCC));
	bbcc->bb = bb;
	bbcc->tid = LPG_(current_tid);

	/* Init pointer caches (LRU) */
	bbcc->lru_next_bbcc = 0;

	LPG_(stat).distinct_bbccs++;

	LPG_DEBUG(3, "  new_bbcc(BB %#lx): %p (now %d)\n", bb_addr(bb), bbcc,
			LPG_(stat).distinct_bbccs);

	return bbcc;
}

/**
 * Inserts a new BBCC into hashes.
 * BBCC specific items must be set as this is used for the hash
 * keys:
 *  fn     : current function
 *  tid    : current thread ID
 *  from   : position where current function is called from
 *
 * Recursion level doesn't need to be set as this is not included
 * in the hash key: Only BBCCs with rec level 0 are in hashes.
 */
static
void insert_bbcc_into_hash(BBCC* bbcc) {
	UInt idx;

	LPG_ASSERT(bbcc->cxt != 0);

	LPG_DEBUG(3, "+ insert_bbcc_into_hash(BB %#lx, fn '%s')\n",
			bb_addr(bbcc->bb), bbcc->cxt->fn[0]->name);

	/* check fill degree of hash and resize if needed (>90%) */
	current_bbccs.entries++;
	if (100 * current_bbccs.entries / current_bbccs.size > 90)
		resize_bbcc_hash();

	idx = bbcc_hash_idx(bbcc->bb, bbcc->cxt, current_bbccs.size);
	bbcc->next = current_bbccs.table[idx];
	current_bbccs.table[idx] = bbcc;

	LPG_DEBUG(3, "- insert_bbcc_into_hash: %u entries\n", current_bbccs.entries);
}

/* Create a new BBCC as a copy of an existing one,
 * but with costs set to 0 and jcc chains empty.
 *
 * This is needed when a BB is executed in another context than
 * the one at instrumentation time of the BB.
 *
 * Use cases:
 *  rec_index == 0: clone from a BBCC with differing tid/cxt
 *                  and insert into hashes
 *  rec_index >0  : clone from a BBCC with same tid/cxt and rec_index 0
 *                  don't insert into hashes
 */
static BBCC* clone_bbcc(BBCC* orig, Context* cxt) {
	BBCC* bbcc;

	LPG_DEBUG(3, "+ clone_bbcc(BB %#lx, fn %s)\n", bb_addr(orig->bb),
			cxt->fn[0]->name);

	bbcc = new_bbcc(orig->bb);

	/* hash insertion is only allowed if tid or cxt is different */
	LPG_ASSERT((orig->tid != LPG_(current_tid)) || (orig->cxt != cxt));

	bbcc->cxt = cxt;
	insert_bbcc_into_hash(bbcc);

	/* update list of BBCCs for same BB */
	bbcc->next_bbcc = orig->bb->bbcc_list;
	orig->bb->bbcc_list = bbcc;

	LPG_DEBUGIF(3)
		LPG_(print_bbcc)(-2, bbcc);

	LPG_(stat).bbcc_clones++;

	return bbcc;
}

/* Get a pointer to the cost centre structure for given basic block
 * address. If created, the BBCC is inserted into the BBCC hash.
 * Also sets BB_seen_before by reference.
 *
 */
BBCC* LPG_(get_bbcc)(BB* bb) {
	BBCC* bbcc;

	LPG_DEBUG(3, "+ get_bbcc(BB %#lx)\n", bb_addr(bb));

	bbcc = bb->bbcc_list;

	if (!bbcc) {
		bbcc = new_bbcc(bb);

		/* initialize BBCC */
		bbcc->cxt = 0;

		bbcc->next_bbcc = bb->bbcc_list;
		bb->bbcc_list = bbcc;
		bb->last_bbcc = bbcc;

		LPG_DEBUGIF(3)
			LPG_(print_bbcc)(-2, bbcc);
	}

	LPG_DEBUG(3, "- get_bbcc(BB %#lx): BBCC %p\n", bb_addr(bb), bbcc);

	return bbcc;
}

/* Callgrind manages its own call stack for each thread.
 * When leaving a function, a underflow can happen when
 * Callgrind's tracing was switched on in the middle of
 * a run, i.e. when Callgrind was not able to trace the
 * call instruction.
 * This function tries to reconstruct the original call.
 * As we know the return address (the address following
 * the CALL instruction), we can detect the function
 * we return back to, but the original call site is unknown.
 * We suppose a call site at return address - 1.
 * (TODO: other heuristic: lookup info of instrumented BBs).
 */
static void handleUnderflow(BB* bb) {
	/* RET at top of call stack */
	BBCC* source_bbcc;
	BB* source_bb;
	Bool seen_before;
	fn_node* caller;
	int fn_number;
	unsigned *pactive;

	LPG_DEBUG(1, "  Callstack underflow !\n");

	/* we emulate an old call from the function we return to
	 * by using (<return address> -1) */
	source_bb = LPG_(get_bb)(bb_addr(bb) - 1, 0, &seen_before);
	source_bbcc = LPG_(get_bbcc)(source_bb);

	/* Force a new top context, will be set active by push_cxt() */
	LPG_(current_fn_stack).top--;
	LPG_(current_state).cxt = 0;
	caller = LPG_(get_fn_node)(bb);
	LPG_(push_cxt)(caller);

	if (!seen_before) {
		LPG_ASSERT(source_bbcc->cxt == 0);
		source_bbcc->cxt = LPG_(current_state).cxt;
		insert_bbcc_into_hash(source_bbcc);
	}
	LPG_ASSERT(LPG_(current_state).bbcc);

	/* correct active counts */
	fn_number = LPG_(current_state).bbcc->cxt->fn[0]->number;
	pactive = LPG_(get_fn_entry)(fn_number);
	(*pactive)--;

	/* This assertion is not correct for reentrant
	 * signal handlers */
	/* LPG_ASSERT(*pactive == 0); */

	/* back to current context */
	LPG_(push_cxt)( LPG_(current_state).bbcc->cxt->fn[0]);
	LPG_(push_call_stack)(source_bbcc, 0, LPG_(current_state).bbcc, (Addr) -1);
}

/*
 * Helper function called at start of each instrumented BB to setup
 * pointer to costs for current thread/context/recursion level
 */
VG_REGPARM(1)
void LPG_(setup_bbcc)(BB* bb) {
	BBCC *bbcc, *last_bbcc;
	Bool call_emulation = False, delayed_push = False;
	Addr sp;
	BB* last_bb;
	ThreadId tid;
	LpgJumpKind jmpkind;
	Bool isConditionalJump;
	Int passed = 0, p, csp;
	Bool ret_without_call = False;
	Int popcount_on_return = 1;
#ifdef LPG_ENABLE_PATH_CACHE
	Int idx;
#endif

	LPG_DEBUG(3, "+ setup_bbcc(BB %#lx)\n", bb_addr(bb));

	/* This is needed because thread switches can not reliable be tracked
	 * with callback LPG_(run_thread) only: we have otherwise no way to get
	 * the thread ID after a signal handler returns.
	 * This could be removed again if that bug is fixed in Valgrind.
	 * This is in the hot path but hopefully not to costly.
	 */
	tid = VG_(get_running_tid)();
#if 1
	/* LPG_(switch_thread) is a no-op when tid is equal to LPG_(current_tid).
	 * As this is on the hot path, we only call LPG_(switch_thread)(tid)
	 * if tid differs from the LPG_(current_tid).
	 */
	if (UNLIKELY(tid != LPG_(current_tid)))
		LPG_(switch_thread)(tid);
#else
	LPG_ASSERT(VG_(get_running_tid)() == LPG_(current_tid));
#endif

	sp = VG_(get_SP)(tid);
	last_bbcc = LPG_(current_state).bbcc;
	last_bb = last_bbcc ? last_bbcc->bb : 0;

	if (last_bb) {
		Int group;

		passed = LPG_(current_state).jmps_passed;
		LPG_ASSERT(passed <= last_bb->cjmp_count);

		jmpkind = last_bb->jmp[passed].jmpkind;
		isConditionalJump = (passed < last_bb->cjmp_count);

		// The first group was already processed in the end of the previous setup_bbcc.
		group = 0;
		for (p = 0; p <= passed; p++) {
#ifdef LPG_ENABLE_PATH_CACHE
			idx = last_bb->jmp[p].dst % PATH_CACHE_SIZE;
			if (!LPG_(current_state).dangling->cache ||
				LPG_(current_state).dangling->cache->phantom[idx].addr != last_bb->jmp[p].dst ||
				LPG_(current_state).dangling->cache->phantom[idx].indirect != last_bb->jmp[p].indirect) {
				LPG_(cfgnode_set_phantom)(LPG_(current_state).cfg,
						LPG_(current_state).dangling, last_bb->jmp[p].dst,
						last_bb->jmp[p].jmpkind, last_bb->jmp[p].indirect);
			}
#else
			LPG_(cfgnode_set_phantom)(LPG_(current_state).cfg,
					LPG_(current_state).dangling, last_bb->jmp[p].dst,
					last_bb->jmp[p].jmpkind, last_bb->jmp[p].indirect);
#endif

			// Only process a new block if it is different from the previous one.
			if (last_bb->jmp[p].group != group) {
				// The next group must be immediately after the previous.
				group++;
				LPG_ASSERT(group == last_bb->jmp[p].group);

#ifdef LPG_ENABLE_PATH_CACHE
				idx = last_bb->groups[group].group_addr % PATH_CACHE_SIZE;
				if (LPG_(current_state).dangling->cache &&
						LPG_(current_state).dangling->cache->block[idx].from == last_bb->groups[group].group_addr &&
						LPG_(current_state).dangling->cache->block[idx].size == last_bb->groups[group].group_size) {
					LPG_(current_state).dangling = LPG_(current_state).dangling->cache->block[idx].to;
				} else {
					LPG_(cfgnode_set_block)(LPG_(current_state).cfg,
							&(LPG_(current_state).dangling), last_bb, group);
				}
#else
				LPG_(cfgnode_set_block)(LPG_(current_state).cfg,
						&(LPG_(current_state).dangling), last_bb, group);
#endif
			}
		}

		// If there are still jumps in the same group, this means
		// that they are phantom nodes.
		while (p <= last_bb->cjmp_count && last_bb->jmp[p].group == group) {
#ifdef LPG_ENABLE_PATH_CACHE
			idx = last_bb->jmp[p].dst % PATH_CACHE_SIZE;
			if (!LPG_(current_state).dangling->cache ||
				LPG_(current_state).dangling->cache->phantom[idx].addr != last_bb->jmp[p].dst ||
				LPG_(current_state).dangling->cache->phantom[idx].indirect != last_bb->jmp[p].indirect) {
				LPG_(cfgnode_set_phantom)(LPG_(current_state).cfg,
						LPG_(current_state).dangling, last_bb->jmp[p].dst,
						last_bb->jmp[p].jmpkind, last_bb->jmp[p].indirect);
			}
#else
			LPG_(cfgnode_set_phantom)(LPG_(current_state).cfg,
					LPG_(current_state).dangling, last_bb->jmp[p].dst,
					last_bb->jmp[p].jmpkind, last_bb->jmp[p].indirect);
#endif

			p++;
		}

		LPG_DEBUGIF(4) {
			LPG_(print_execstate)(-2, &LPG_(current_state));
		}
	} else {
		jmpkind = jk_None;
		isConditionalJump = False;

		LPG_(current_state).cfg = LPG_(get_cfg)(bb->groups[0].group_addr);
		LPG_(current_state).dangling = 0;
	}

	/* Manipulate JmpKind if needed, only using BB specific info */
	csp = LPG_(current_call_stack).sp;

	/* A return not matching the top call in our callstack is a jump */
	if ((jmpkind == jk_Return) && (csp > 0)) {
		Int csp_up = csp - 1;
		call_entry* top_ce = &(LPG_(current_call_stack).entry[csp_up]);

		/* We have a real return if
		 * - the stack pointer (SP) left the current stack frame, or
		 * - SP has the same value as when reaching the current function
		 *   and the address of this BB is the return address of last call
		 *   (we even allow to leave multiple frames if the SP stays the
		 *    same and we find a matching return address)
		 * The latter condition is needed because on PPC, SP can stay
		 * the same over CALL=b(c)l / RET=b(c)lr boundaries
		 */
		if (sp < top_ce->sp)
			popcount_on_return = 0;
		else if (top_ce->sp == sp) {
			while (1) {
				if (top_ce->ret_addr == bb_addr(bb))
					break;
				if (csp_up > 0) {
					csp_up--;
					top_ce = &(LPG_(current_call_stack).entry[csp_up]);
					if (top_ce->sp == sp) {
						popcount_on_return++;
						continue;
					}
				}
				popcount_on_return = 0;
				break;
			}
		}
		if (popcount_on_return == 0) {
			jmpkind = jk_Jump;
			ret_without_call = True;
		}
	}

	/* Should this jump be converted to call or pop/call ? */
	if ((jmpkind != jk_Return) && (jmpkind != jk_Call) && last_bb) {
		/* We simulate a JMP/Cont to be a CALL if
		 * - jump is in another ELF object or section kind
		 * - jump is to first instruction of a function (tail recursion)
		 */
		if (ret_without_call ||
		/* This is for detection of optimized tail recursion.
		 * On PPC, this is only detected as call when going to another
		 * function. The problem is that on PPC it can go wrong
		 * more easily (no stack frame setup needed)
		 */
#if defined(VGA_ppc32)
				(bb->is_entry && (last_bb->fn != bb->fn)) ||
#else
				bb->is_entry ||
#endif
				(last_bb->sect_kind != bb->sect_kind)
				|| (last_bb->obj->number != bb->obj->number)) {

			LPG_DEBUG(1, "     JMP: %s[%s] to %s[%s]%s!\n", last_bb->fn->name,
					last_bb->obj->name, bb->fn->name, bb->obj->name,
					ret_without_call ? " (RET w/o CALL)" : "");

			jmpkind = jk_Call;
			call_emulation = True;
		}
	}

	LPG_DEBUGIF(1) {
		if (isConditionalJump)
			VG_(printf)("Cond-");
		switch (jmpkind) {
		case jk_None:
			VG_(printf)("Fall-through");
			break;
		case jk_Jump:
			VG_(printf)("Jump");
			break;
		case jk_Call:
			VG_(printf)("Call");
			break;
		case jk_Return:
			VG_(printf)("Return");
			break;
		default:
			tl_assert(0);
		}
		VG_(printf)(" %08lx -> %08lx, SP %08lx\n",
				last_bb ? bb_jmpaddr(last_bb) : 0, bb_addr(bb), sp);
	}

	/* Handle CALL/RET and update context to get correct BBCC */

	if (jmpkind == jk_Return) {
		if ((csp == 0)
				|| ((LPG_(current_fn_stack).top > LPG_(current_fn_stack).bottom)
						&& (*(LPG_(current_fn_stack).top - 1) == 0))) {

			/* On an empty call stack or at a signal separation marker,
			 * a RETURN generates an call stack underflow.
			 */
			handleUnderflow(bb);
			LPG_(pop_call_stack)(False);
		} else {
			LPG_ASSERT(popcount_on_return > 0);
			LPG_(unwind_call_stack)(sp, popcount_on_return);
		}
	} else {
		Int unwind_count = LPG_(unwind_call_stack)(sp, 0);
		if (unwind_count > 0) {
			/* if unwinding was done, this actually is a return */
			jmpkind = jk_Return;
		}

		if (jmpkind == jk_Call) {
			delayed_push = True;

			csp = LPG_(current_call_stack).sp;
			if (call_emulation && csp > 0)
				sp = LPG_(current_call_stack).entry[csp - 1].sp;
		}
	}

	/* Change new context if needed, taking delayed_push into account */
	if (delayed_push || LPG_(current_state).cxt == 0) {
		LPG_(push_cxt)(LPG_(get_fn_node)(bb));
	}
	LPG_ASSERT(LPG_(current_fn_stack).top > LPG_(current_fn_stack).bottom);

	/* If there is a fresh instrumented BBCC, assign current context */
	bbcc = LPG_(get_bbcc)(bb);
	if (bbcc->cxt == 0) {
		bbcc->cxt = LPG_(current_state).cxt;
		insert_bbcc_into_hash(bbcc);
	} else {
		/* get BBCC with current context */

		/* first check LRU of last bbcc executed */

		if (last_bbcc) {
			bbcc = last_bbcc->lru_next_bbcc;
			if (bbcc
					&& ((bbcc->bb != bb)
							|| (bbcc->cxt != LPG_(current_state).cxt)))
				bbcc = 0;
		} else
			bbcc = 0;

		if (!bbcc)
			bbcc = lookup_bbcc(bb, LPG_(current_state).cxt);
		if (!bbcc)
			bbcc = clone_bbcc(bb->bbcc_list, LPG_(current_state).cxt);

		bb->last_bbcc = bbcc;
	}

	/* save for fast lookup */
	if (last_bbcc)
		last_bbcc->lru_next_bbcc = bbcc;

	if (delayed_push)
		LPG_(push_call_stack)(LPG_(current_state).bbcc, passed, bbcc, sp);

	LPG_(current_state).bbcc = bbcc;
	/* Even though this will be set in instrumented code directly before
	 * side exits, it needs to be set to 0 here in case an exception
	 * happens in first instructions of the BB */
	LPG_(current_state).jmps_passed = 0;

#ifdef LPG_ENABLE_PATH_CACHE
	idx = bb->groups[0].group_addr % PATH_CACHE_SIZE;
	if (LPG_(current_state).dangling && LPG_(current_state).dangling->cache &&
			LPG_(current_state).dangling->cache->block[idx].from == bb->groups[0].group_addr &&
			LPG_(current_state).dangling->cache->block[idx].size == bb->groups[0].group_size) {
		LPG_(current_state).dangling = LPG_(current_state).dangling->cache->block[idx].to;
	} else {
		LPG_(cfgnode_set_block)(LPG_(current_state).cfg,
				&(LPG_(current_state).dangling), bb, 0);
	}
#else
	LPG_(cfgnode_set_block)(LPG_(current_state).cfg,
			&(LPG_(current_state).dangling), bb, 0);
#endif

	LPG_DEBUGIF(1) {
		VG_(printf)("     ");
		LPG_(print_bbcc_fn)(bbcc);
		VG_(printf)("\n");
	}

	LPG_DEBUG(3,
			"- setup_bbcc (BB %#lx): Instrs %u (Len %u)\n",
			bb_addr(bb), bb->instr_count,
			bb->instr_len);
	LPG_DEBUGIF(3)
		LPG_(print_cxt)(-8, LPG_(current_state).cxt);
	LPG_DEBUG(3, "\n");

	LPG_(stat).bb_executions++;
}
