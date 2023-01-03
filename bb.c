/*--------------------------------------------------------------------*/
/*--- CFGgrind                                                     ---*/
/*---                                                         bb.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of CFGgrind, a dynamic control flow graph (CFG)
   reconstruction tool.

   Copyright (C) 2023, Andrei Rimsa (andrei@cefetmg.br)

   This tool is derived and contains lot of code from Callgrind
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "global.h"
#include "pub_tool_threadstate.h" // for VG_(get_running_tid)()

/*------------------------------------------------------------*/
/*--- Basic block (BB) operations                          ---*/
/*------------------------------------------------------------*/

/* BB hash, resizable */
bb_hash bbs;

void CGD_(init_bb_hash)() {
   Int i;

   bbs.size    = 8437;
   bbs.entries = 0;
   bbs.table = (BB**) CGD_MALLOC("cgd.bb.ibh.1",
                                 bbs.size * sizeof(BB*));

   for (i = 0; i < bbs.size; i++)
	   bbs.table[i] = NULL;
}

void CGD_(destroy_bb_hash)() {
	Int i;
	Int size;

	for (i = 0; i < bbs.size; i++) {
		BB* bb = bbs.table[i];
		while (bb) {
			BB* next = bb->next;

			size = sizeof(BB)
				+ bb->instr_count * sizeof(InstrInfo)
				+ (bb->cjmp_count+1) * sizeof(CJmpInfo)
				+ bb->groups_count * sizeof(InstrGroupInfo);
			CGD_DATA_FREE(bb, size);

			bb = next;
			bbs.entries--;
		}
	}

	CGD_ASSERT(bbs.entries == 0);

	CGD_FREE(bbs.table);
	bbs.table = 0;
}

bb_hash* CGD_(get_bb_hash)()
{
  return &bbs;
}

/* The hash stores BBs according to
 * - ELF object (is 0 for code in anonymous mapping)
 * - BB base as object file offset
 */
static __inline__
UInt bb_hash_idx(obj_node* obj, PtrdiffT offset, UInt size)
{
  return (((Addr)obj) + offset) % size;
}

/* double size of bb table  */
static
void resize_bb_table(void)
{
    Int i, new_size, conflicts1 = 0, conflicts2 = 0;
    BB **new_table, *curr, *next;
    UInt new_idx;

    new_size  = 2* bbs.size +3;
    new_table = (BB**) CGD_MALLOC("cgd.bb.rbt.1",
                                  new_size * sizeof(BB*));
 
    for (i = 0; i < new_size; i++)
      new_table[i] = NULL;
 
    for (i = 0; i < bbs.size; i++) {
	if (bbs.table[i] == NULL) continue;
 
	curr = bbs.table[i];
	while (NULL != curr) {
	    next = curr->next;

	    new_idx = bb_hash_idx(curr->obj, curr->offset, new_size);

	    curr->next = new_table[new_idx];
	    new_table[new_idx] = curr;
	    if (curr->next) {
		conflicts1++;
		if (curr->next->next)
		    conflicts2++;
	    }

	    curr = next;
	}
    }

    CGD_FREE(bbs.table);

    CGD_DEBUG(0, "Resize BB Hash: %u => %d (entries %u, conflicts %d/%d)\n",
	     bbs.size, new_size,
	     bbs.entries, conflicts1, conflicts2);

    bbs.size  = new_size;
    bbs.table = new_table;
    CGD_(stat).bb_hash_resizes++;
}


/**
 * Allocate new BB structure (including space for event type list)
 * Not initialized:
 * - instr_len, instr[]
 */
static BB* new_bb(obj_node* obj, PtrdiffT offset,
		  UInt instr_count, UInt cjmp_count, Bool cjmp_inverted, UInt groups_count)
{
   BB* bb;
   UInt idx, size;

   // Remove me later.
   CGD_ASSERT(groups_count > 0);

   /* check fill degree of bb hash table and resize if needed (>80%) */
   bbs.entries++;
   if (10 * bbs.entries / bbs.size > 8)
       resize_bb_table();

   size = sizeof(BB) + instr_count * sizeof(InstrInfo)
                     + (cjmp_count+1) * sizeof(CJmpInfo)
                     + groups_count * sizeof(InstrGroupInfo);
   bb = (BB*) CGD_MALLOC("cgd.bb.nb.1", size);
   VG_(memset)(bb, 0, size);

   bb->obj        = obj;
   bb->offset     = offset;
   
   bb->instr_count = instr_count;
   bb->cjmp_count  = cjmp_count;
   bb->cjmp_inverted = cjmp_inverted;
   bb->jmp         = (CJmpInfo*) &(bb->instr[instr_count]);
   bb->instr_len   = 0;
   bb->sect_kind   = VG_(DebugInfo_sect_kind)(NULL, offset + obj->offset);
   bb->fn          = 0;
   bb->line        = 0;
   bb->is_entry    = 0;

   bb->groups = (InstrGroupInfo*) &(bb->jmp[cjmp_count+1]);
   bb->groups_count = groups_count;

   /* insert into BB hash table */
   idx = bb_hash_idx(obj, offset, bbs.size);
   bb->next = bbs.table[idx];
   bbs.table[idx] = bb;

   CGD_(stat).distinct_bbs++;

#if CGD_ENABLE_DEBUG
   CGD_DEBUGIF(3) {
     VG_(printf)("  new_bb (instr %u, jmps %u, inv %s) [now %d]: ",
		 instr_count, cjmp_count,
		 cjmp_inverted ? "yes":"no",
		 CGD_(stat).distinct_bbs);
      CGD_(print_bb)(0, bb);
      VG_(printf)("\n");
   }
#endif

   CGD_(get_fn_node)(bb);

   return bb;
}


/* get the BB structure for a BB start address */
static __inline__
BB* lookup_bb(obj_node* obj, PtrdiffT offset)
{
    BB* bb;
    Int idx;

    idx = bb_hash_idx(obj, offset, bbs.size);
    bb = bbs.table[idx];

    while(bb) {
      if ((bb->obj == obj) && (bb->offset == offset)) break;
      bb = bb->next;
    }

    CGD_DEBUG(5, "  lookup_bb (Obj %s, off %#lx): %p\n",
              obj->name, (UWord)offset, bb);
    return bb;
}

static __inline__
obj_node* obj_of_address(Addr addr)
{
  obj_node* obj;
  DebugInfo* di;
  PtrdiffT offset;

  DiEpoch ep = VG_(current_DiEpoch)();
  di = VG_(find_DebugInfo)(ep, addr);
  obj = CGD_(get_obj_node)( di );

  /* Update symbol offset in object if remapped */
  /* FIXME (or at least check this) 2008 Feb 19: 'offset' is
     only correct for text symbols, not for data symbols */
  offset = di ? VG_(DebugInfo_get_text_bias)(di):0;
  if (obj->offset != offset) {
      Addr start = di ? VG_(DebugInfo_get_text_avma)(di) : 0;

      CGD_DEBUG(0, "Mapping changed for '%s': %#lx -> %#lx\n",
		obj->name, obj->start, start);

      /* Size should be the same, and offset diff == start diff */
      CGD_ASSERT( obj->size == (di ? VG_(DebugInfo_get_text_size)(di) : 0) );
      CGD_ASSERT( obj->start - start == obj->offset - offset );
      obj->offset = offset;
      obj->start = start;
  }

  return obj;
}

/* Get the BB structure for a BB start address.
 * If the BB has to be created, the IRBB is needed to
 * compute the event type list for costs, and seen_before is
 * set to False. Otherwise, seen_before is set to True.
 *
 * BBs are never discarded. There are 2 cases where this function
 * is called from CGD_(instrument)() and a BB already exists:
 * - The instrumented version was removed from Valgrinds TT cache
 * - The ELF object of the BB was unmapped and mapped again.
 *   This involves a possibly different address, but is handled by
 *   looking up a BB keyed by (obj_node, file offset).
 *
 * bbIn==0 is possible for artificial BB without real code.
 * Such a BB is created when returning to an unknown function.
 */
BB* CGD_(get_bb)(Addr addr, IRSB* bbIn, /*OUT*/ Bool *seen_before)
{
  BB*   bb;
  obj_node* obj;
  UInt n_instrs, n_jmps, n_groups;
  Bool cjmp_inverted = False;

  CGD_DEBUG(5, "+ get_bb(BB %#lx)\n", addr);

  obj = obj_of_address(addr);
  bb = lookup_bb(obj, addr - obj->offset);

  n_instrs = 0;
  n_jmps = 0;
  n_groups = 0;
  CGD_(collectBlockInfo)(bbIn, &n_instrs, &n_jmps, &cjmp_inverted, &n_groups);

  *seen_before = bb ? True : False;
  if (*seen_before) {
    if (bb->instr_count != n_instrs) {
      VG_(message)(Vg_DebugMsg, 
		   "ERROR: BB Retranslation Mismatch at BB %#lx\n", addr);
      VG_(message)(Vg_DebugMsg,
		   "  new: Obj %s, Off %#lx, BBOff %#lx, Instrs %u\n",
		   obj->name, (UWord)obj->offset,
		   addr - obj->offset, n_instrs);
      VG_(message)(Vg_DebugMsg,
		   "  old: Obj %s, Off %#lx, BBOff %#lx, Instrs %u\n",
		   bb->obj->name, (UWord)bb->obj->offset,
		   (UWord)bb->offset, bb->instr_count);
      CGD_ASSERT(bb->instr_count == n_instrs);
    }
    CGD_ASSERT(bb->cjmp_count == n_jmps);
    CGD_ASSERT(bb->groups_count == n_groups);
    CGD_(stat).bb_retranslations++;

    CGD_DEBUG(5, "- get_bb(BB %#lx): seen before.\n", addr);
    return bb;
  }

  bb = new_bb(obj, addr - obj->offset, n_instrs, n_jmps, cjmp_inverted, n_groups);

  CGD_DEBUG(5, "- get_bb(BB %#lx)\n", addr);

  return bb;
}

/* Delete the BB info for the bb with unredirected entry-point
   address 'addr'. */
void CGD_(delete_bb)(Addr addr)
{
    BB  *bb, *bp;
    Int idx, size;

    obj_node* obj = obj_of_address(addr);
    PtrdiffT offset = addr - obj->offset;

    idx = bb_hash_idx(obj, offset, bbs.size);
    bb = bbs.table[idx];

    /* bb points at the current bb under consideration, and bp is the
       one before. */
    bp = NULL;
    while(bb) {
      if ((bb->obj == obj) && (bb->offset == offset)) break;
      bp = bb;
      bb = bb->next;
    }

    if (bb == NULL) {
		CGD_DEBUG(3, "  delete_bb (Obj %s, off %#lx): NOT FOUND\n",
			  obj->name, (UWord)offset);

		/* we didn't find it. */
		return;
    }

    /* unlink it from hash table */

    if (bp == NULL) {
       /* we found the first one in the list. */
       tl_assert(bb == bbs.table[idx]);
       bbs.table[idx] = bb->next;
    } else {
       tl_assert(bb != bbs.table[idx]);
       bp->next = bb->next;
    }

    CGD_DEBUG(3, "  delete_bb (Obj %s, off %#lx): %p\n",
	      obj->name, (UWord)offset, bb);

    // FIXME: We may be using this BB somewhere else.
	/* Fill the block up with junk and then free it, so we will
	   hopefully get a segfault if it is used again by mistake. */
	size = sizeof(BB)
		+ bb->instr_count * sizeof(InstrInfo)
		+ (bb->cjmp_count+1) * sizeof(CJmpInfo)
		+ bb->groups_count * sizeof(InstrGroupInfo);
	CGD_DATA_FREE(bb, size);
	bbs.entries--;
}

/*
 * Helper function called at start of each instrumented BB.
 */
VG_REGPARM(1)
void CGD_(setup_bb)(BB* bb) {
	Bool call_emulation = False, delayed_push = False;
	Addr sp;
	BB* last_bb;
	ThreadId tid;
	BBJumpKind jmpkind;
	Bool isConditionalJump;
	Int passed = 0, p, csp;
	Bool ret_without_call = False;
	Int popcount_on_return = 1;
#if CFG_NODE_CACHE_SIZE > 0
	CfgNodeBlockCache* blockCache;
	CfgNodePhantomCache* phantomCache;
#endif

	CGD_DEBUG(3, "+ setup_bb(BB %#lx)\n", bb_addr(bb));

	/* This is needed because thread switches can not reliable be tracked
	 * with callback CGD_(run_thread) only: we have otherwise no way to get
	 * the thread ID after a signal handler returns.
	 * This could be removed again if that bug is fixed in Valgrind.
	 * This is in the hot path but hopefully not to costly.
	 */
	tid = VG_(get_running_tid)();
#if 1
	/* CGD_(switch_thread) is a no-op when tid is equal to CGD_(current_tid).
	 * As this is on the hot path, we only call CGD_(switch_thread)(tid)
	 * if tid differs from the CGD_(current_tid).
	 */
	if (UNLIKELY(tid != CGD_(current_tid)))
		CGD_(switch_thread)(tid);
#else
	CGD_ASSERT(VG_(get_running_tid)() == CGD_(current_tid));
#endif

	sp = VG_(get_SP)(tid);
	last_bb = CGD_(current_state).bb;

	if (last_bb) {
		Int group;

		passed = CGD_(current_state).jmps_passed;
		CGD_ASSERT(passed <= last_bb->cjmp_count);

		jmpkind = last_bb->jmp[passed].jmpkind;
		isConditionalJump = (passed < last_bb->cjmp_count);

		// The first group was already processed in the end of the previous setup_bbcc.
		group = 0;
		for (p = 0; p <= passed; p++) {
			// Only process a new block if it is different from the previous one.
			if (last_bb->jmp[p].group != group) {
				// The next group must be immediately after the previous.
				group++;
				CGD_ASSERT(group == last_bb->jmp[p].group);

#if CFG_NODE_CACHE_SIZE > 0
				blockCache = CGD_(current_state).working->cache.block ?
						&(CGD_(current_state).working->cache.block[CFG_NODE_CACHE_INDEX(last_bb->groups[group].group_addr)]) : 0;
				if (blockCache &&
						blockCache->addr == last_bb->groups[group].group_addr &&
						blockCache->size == last_bb->groups[group].group_size) {
#if ENABLE_PROFILING
					blockCache->count++;
#endif // ENABLE_PROFILING
					CGD_(current_state).working = blockCache->working;
				} else {
#if ENABLE_PROFILING
					if (blockCache && blockCache->count > 0)
						CGD_(cfgnode_flush_edge_count)(CGD_(current_state).cfg,
							CGD_(current_state).working, blockCache);
#endif // ENABLE_PROFILING
#endif // CFG_NODE_CACHE_SIZE
					CGD_(current_state).working = CGD_(cfgnode_set_block)(CGD_(current_state).cfg,
							CGD_(current_state).working, last_bb, group);
#if CFG_NODE_CACHE_SIZE > 0
				}
#endif
			}

#if CFG_NODE_CACHE_SIZE > 0
			phantomCache = CGD_(current_state).working->cache.phantom ?
					&(CGD_(current_state).working->cache.phantom[CFG_NODE_CACHE_INDEX(last_bb->jmp[p].dst)]) : 0;
			if (!phantomCache ||
					phantomCache->addr != last_bb->jmp[p].dst ||
					phantomCache->indirect != last_bb->jmp[p].indirect)
#endif
				CGD_(cfgnode_set_phantom)(CGD_(current_state).cfg,
						CGD_(current_state).working, last_bb->jmp[p].dst,
						last_bb->jmp[p].jmpkind, last_bb->jmp[p].indirect);
		}

		// If there are still jumps in the same group, this means
		// that they are phantom nodes.
		while (p <= last_bb->cjmp_count && last_bb->jmp[p].group == group) {
#if CFG_NODE_CACHE_SIZE > 0
			phantomCache = CGD_(current_state).working->cache.phantom ?
					&(CGD_(current_state).working->cache.phantom[CFG_NODE_CACHE_INDEX(last_bb->jmp[p].dst)]) : 0;

			if (!phantomCache ||
					phantomCache->addr != last_bb->jmp[p].dst ||
					phantomCache->indirect != last_bb->jmp[p].indirect)
#endif
				CGD_(cfgnode_set_phantom)(CGD_(current_state).cfg,
						CGD_(current_state).working, last_bb->jmp[p].dst,
						last_bb->jmp[p].jmpkind, last_bb->jmp[p].indirect);

			p++;
		}

		CGD_DEBUGIF(4) {
			CGD_(print_execstate)(-2, &CGD_(current_state));
		}
	} else {
		CFG* called;

		jmpkind = bjk_None;
		isConditionalJump = False;

		called = CGD_(get_cfg)(bb->groups[0].group_addr);
		if (CGD_(current_state).sig > 0)
			CGD_(cfgnode_set_signal_handler)(CGD_(current_state).cfg,
				CGD_(current_state).working, called, CGD_(current_state).sig);

		CGD_(current_state).cfg = called;
		CGD_(current_state).working = CGD_(cfg_entry_node)(called);

#if ENABLE_PROFILING
		CGD_(current_state).cfg->stats.execs++;
#endif
	}

	/* Manipulate JmpKind if needed, only using BB specific info */
	csp = CGD_(current_call_stack).sp;

	/* A return not matching the top call in our callstack is a jump */
	if ((jmpkind == bjk_Return) && (csp > 0)) {
		Int csp_up = csp - 1;
		call_entry* top_ce = &(CGD_(current_call_stack).entry[csp_up]);

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
					top_ce = &(CGD_(current_call_stack).entry[csp_up]);
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
			jmpkind = bjk_Jump;
			ret_without_call = True;
		}
	}

	/* Should this jump be converted to call or pop/call ? */
	if (CGD_(clo).emulate_calls && last_bb &&
			jmpkind != bjk_Return && jmpkind != bjk_Call) {
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

			CGD_DEBUG(1, "     JMP: %s[%s] to %s[%s]%s!\n", last_bb->fn->name,
					last_bb->obj->name, bb->fn->name, bb->obj->name,
					ret_without_call ? " (RET w/o CALL)" : "");

			jmpkind = bjk_Call;
			call_emulation = True;
		}
	}

	CGD_DEBUGIF(1) {
		if (isConditionalJump)
			VG_(printf)("Cond-");
		switch (jmpkind) {
		case bjk_None:
			VG_(printf)("Fall-through");
			break;
		case bjk_Jump:
			VG_(printf)("Jump");
			break;
		case bjk_Call:
			VG_(printf)("Call");
			break;
		case bjk_Return:
			VG_(printf)("Return");
			break;
		default:
			tl_assert(0);
		}
		VG_(printf)(" %08lx -> %08lx, SP %08lx\n",
				last_bb ? bb_jmpaddr(last_bb) : 0, bb_addr(bb), sp);
	}

	/* Handle CALL/RET */
	if (jmpkind == bjk_Return) {
		// If it has nothing to pop, consider it a jump.
		if (csp != 0 && popcount_on_return > 0)
			CGD_(unwind_call_stack)(sp, popcount_on_return);
	} else {
		Int unwind_count = CGD_(unwind_call_stack)(sp, 0);
		if (unwind_count > 0) {
			/* if unwinding was done, this actually is a return */
			jmpkind = bjk_Return;
		}

		if (jmpkind == bjk_Call) {
			delayed_push = True;

			csp = CGD_(current_call_stack).sp;
			if (call_emulation && csp > 0)
				sp = CGD_(current_call_stack).entry[csp - 1].sp;
		}
	}

	if (delayed_push) {
		if (call_emulation)
			CGD_(cfgnode_remove_successor_with_addr)(CGD_(current_state).cfg,
					CGD_(current_state).working, bb->groups[0].group_addr);

		CGD_(push_call_stack)(last_bb, passed, bb, sp);
	}

#if CFG_NODE_CACHE_SIZE > 0
	blockCache = CGD_(current_state).working->cache.block ?
			&(CGD_(current_state).working->cache.block[CFG_NODE_CACHE_INDEX(bb->groups[0].group_addr)]) : 0;
	if (blockCache &&
			blockCache->addr == bb->groups[0].group_addr &&
			blockCache->size == bb->groups[0].group_size) {
#if ENABLE_PROFILING
		blockCache->count++;
#endif // ENABLE_PROFILING
		CGD_(current_state).working = blockCache->working;
	} else {
#if ENABLE_PROFILING
		if (blockCache && blockCache->count > 0)
			CGD_(cfgnode_flush_edge_count)(CGD_(current_state).cfg,
				CGD_(current_state).working, blockCache);
#endif // ENABLE_PROFILING
#endif // CFG_NODE_CACHE_SIZE
		CGD_(current_state).working = CGD_(cfgnode_set_block)(CGD_(current_state).cfg,
				CGD_(current_state).working, bb, 0);
#if CFG_NODE_CACHE_SIZE > 0
	}
#endif

	CGD_(current_state).bb = bb;
	/* Even though this will be set in instrumented code directly before
	 * side exits, it needs to be set to 0 here in case an exception
	 * happens in first instructions of the BB */
	CGD_(current_state).jmps_passed = 0;

	CGD_DEBUG(3,
			"- setup_bb (BB %#lx): Instrs %u (Len %u)\n",
			bb_addr(bb), bb->instr_count, bb->instr_len);

	CGD_(stat).bb_executions++;
}
