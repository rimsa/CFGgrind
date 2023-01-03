/*--------------------------------------------------------------------*/
/*--- CFGgrind                                                     ---*/
/*---                                                       main.c ---*/
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

#include "config.h"
#include "global.h"

#include "pub_tool_threadstate.h"
#include "pub_tool_transtab.h"       // VG_(discard_translations_safely)

/*------------------------------------------------------------*/
/*--- Global variables                                     ---*/
/*------------------------------------------------------------*/

/* for all threads */
CommandLineOptions CGD_(clo);
Statistics CGD_(stat);

/* thread and signal handler specific */
exec_state CGD_(current_state);

/*------------------------------------------------------------*/
/*--- Statistics                                           ---*/
/*------------------------------------------------------------*/

static void CGD_(init_statistics)(Statistics* s) {
	s->bb_executions = 0;

	s->bb_retranslations = 0;

	s->distinct_objs = 0;
	s->distinct_files = 0;
	s->distinct_fns = 0;
	s->distinct_bbs = 0;
	s->distinct_instrs = 0;
	s->distinct_groups = 0;
	s->distinct_cfgs = 0;
	s->distinct_cfg_nodes = 0;

	s->bb_hash_resizes = 0;
	s->call_stack_resizes = 0;
	s->cfg_hash_resizes = 0;
	s->instrs_pool_resizes = 0;

	s->full_debug_BBs = 0;
	s->file_line_debug_BBs = 0;
	s->fn_name_debug_BBs = 0;
	s->no_debug_BBs = 0;
}

/* A struct which holds all the running state during instrumentation.
 Mostly to avoid passing loads of parameters everywhere. */
typedef struct {
	/* The array of InstrInfo's is part of BB struct. */
	BB* bb;

	/* BB seen before (ie. re-instrumentation) */
	Bool seen_before;

	/* Number InstrInfo bins 'used' so far. */
	UInt ii_index;

	/* Number InstrGroupInfo bins 'used' so far. */
	UInt ig_index;

	// current offset of guest instructions from BB start
	UInt instr_offset;

	/* The output SB being constructed. */
	IRSB* sbOut;
} CDG_State;

/* Initialise or check (if already seen before) an InstrInfo for next insn.
 We only can set instr_offset/instr_size here. The required event set and
 resulting cost offset depend on events (Ir/Dr/Dw/Dm) in guest
 instructions. The event set is extended as required on flush of the event
 queue (when Dm events were determined), cost offsets are determined at
 end of BB instrumentation. */
static InstrInfo* next_InstrInfo(CDG_State* cdgs, UInt instr_size) {
	InstrInfo* ii;

	tl_assert(cdgs->ii_index >= 0);
	tl_assert(cdgs->ii_index < cdgs->bb->instr_count);
	ii = &cdgs->bb->instr[cdgs->ii_index];

	if (cdgs->seen_before) {
		CGD_ASSERT(ii->instr_offset == cdgs->instr_offset);
		CGD_ASSERT(ii->instr_size == instr_size);
	} else {
		ii->instr_offset = cdgs->instr_offset;
		ii->instr_size = instr_size;
	}

	cdgs->ii_index++;
	cdgs->instr_offset += instr_size;
	CGD_(stat).distinct_instrs++;

	return ii;
}

static InstrGroupInfo* next_InstrGroupInfo(CDG_State* cdgs) {
	InstrGroupInfo* ig;
	Addr addr;

	tl_assert(cdgs->ig_index >= 0 && cdgs->ig_index < cdgs->bb->groups_count);
	ig = &cdgs->bb->groups[cdgs->ig_index];

	addr = bb_addr(cdgs->bb) + cdgs->instr_offset;
	if (cdgs->seen_before) {
		CGD_ASSERT(ig->group_addr == addr);
		CGD_ASSERT(ig->bb_info.first_instr == cdgs->ii_index);
	} else {
		ig->group_addr = addr;
		ig->group_size = 0;
		ig->instr_count = 0;
		ig->bb_info.first_instr = cdgs->ii_index;
		ig->bb_info.last_instr = 0;
	}

	cdgs->ig_index++;
	CGD_(stat).distinct_groups++;

	return ig;
}

/*------------------------------------------------------------*/
/*--- Instrumentation                                      ---*/
/*------------------------------------------------------------*/

#if defined(VG_BIGENDIAN)
# define CGD_Endness Iend_BE
#elif defined(VG_LITTLEENDIAN)
# define CGD_Endness Iend_LE
#else
# error "Unknown endianness"
#endif

static Addr IRConst2Addr(IRConst* con) {
	Addr addr;

	if (sizeof(RegWord) == 4) {
		CGD_ASSERT(con->tag == Ico_U32);
		addr = con->Ico.U32;
	} else if (sizeof(RegWord) == 8) {
		CGD_ASSERT(con->tag == Ico_U64);
		addr = con->Ico.U64;
	} else
		VG_(tool_panic)("cfggrind: invalid addr type");

	return addr;
}

/* First pass over a BB to instrument, counting instructions and jumps
 * This is needed for the size of the BB struct to allocate
 *
 * Called from CGD_(get_bb)
 */
void CGD_(collectBlockInfo)(IRSB* sbIn,
		/*INOUT*/UInt* instrs,
		/*INOUT*/UInt* cjmps,
		/*INOUT*/Bool* cjmp_inverted,
		/*INOUT*/UInt *groups) {

	Int i;
	IRStmt* st;
	Addr instrAddr = 0, jumpDst;
	UInt instrLen = 0;
	Bool toNextInstr = False;
	Bool nextGroup = True;

	// Ist_Exit has to be ignored in preamble code, before first IMark:
	// preamble code is added by VEX for self modifying code, and has
	// nothing to do with client code
	Bool inPreamble = True;

	if (!sbIn)
		return;

	for (i = 0; i < sbIn->stmts_used; i++) {
		st = sbIn->stmts[i];
		if (Ist_IMark == st->tag) {
			inPreamble = False;

			instrAddr = st->Ist.IMark.addr;
			instrLen = st->Ist.IMark.len;

			(*instrs)++;
			toNextInstr = False;

			if (nextGroup) {
				(*groups)++;
				nextGroup = False;
			}
		}
		if (inPreamble)
			continue;
		if (Ist_Exit == st->tag) {
			jumpDst = IRConst2Addr(st->Ist.Exit.dst);
			toNextInstr = (jumpDst == instrAddr + instrLen);

			(*cjmps)++;

			nextGroup = True;
		}
	}

	/* if the last instructions of BB conditionally jumps to next instruction
	 * (= first instruction of next BB in memory), this is a inverted by VEX.
	 */
	*cjmp_inverted = toNextInstr;
}

static
void addConstMemStoreStmt(IRSB* bbOut, UWord addr, UInt val, IRType hWordTy) {
	addStmtToIRSB(bbOut,
			IRStmt_Store(CGD_Endness,
					IRExpr_Const(
							hWordTy == Ity_I32 ?
									IRConst_U32(addr) : IRConst_U64(addr)),
					IRExpr_Const(IRConst_U32(val))));
}

/* add helper call to setup_bb, with pointer to BB struct as argument
 *
 * precondition for setup_bb:
 * - jmps_passed has number of cond.jumps passed in last executed BB
 * - current_bbcc has a pointer to the BBCC of the last executed BB
 *   Thus, if bbcc_jmpkind is != -1 (JmpNone),
 *     current_bbcc->bb->jmp_addr
 *   gives the address of the jump source.
 *
 * the setup does 2 things:
 * - trace call:
 *   * Unwind own call stack, i.e sync our ESP with real ESP
 *     This is for ESP manipulation (longjmps, C++ exec handling) and RET
 *   * For CALLs or JMPs crossing objects, record call arg +
 *     push are on own call stack
 *
 * - prepare for cache log functions:
 *   set current_bbcc to BBCC that gets the costs for this BB execution
 *   attached
 */
static
void addBBSetupCall(CDG_State* cdgs) {
	IRDirty* di;
	IRExpr *arg1, **argv;

	arg1 = mkIRExpr_HWord((HWord) cdgs->bb);
	argv = mkIRExprVec_1(arg1);
	di = unsafeIRDirty_0_N(1, "setup_bb",
			VG_(fnptr_to_fnentry)(&CGD_(setup_bb)), argv);
	addStmtToIRSB(cdgs->sbOut, IRStmt_Dirty(di));
}

static IRSB* CGD_(instrument)(VgCallbackClosure* closure, IRSB* sbIn,
		const VexGuestLayout* layout, const VexGuestExtents* vge,
		const VexArchInfo* archinfo_host, IRType gWordTy, IRType hWordTy) {
	Int i;
	IRStmt* st;
	Addr origAddr;
	InstrInfo* curr_inode = NULL;
	Bool nextGroup = True;
	InstrGroupInfo* curr_group = NULL;
	CDG_State cdgs;
	UInt cJumps = 0;
	Int instr_stmt_count = 0;

	if (gWordTy != hWordTy) {
		/* We don't currently support this case. */
		VG_(tool_panic)("host/guest word size mismatch");
	}

	CGD_DEBUG(3, "+ instrument(BB %#lx)\n", (Addr )closure->readdr);

	/* Set up SB for instrumented IR */
	cdgs.sbOut = deepCopyIRSBExceptStmts(sbIn);

	// Copy verbatim any IR preamble preceding the first IMark
	i = 0;
	while (i < sbIn->stmts_used && sbIn->stmts[i]->tag != Ist_IMark) {
		addStmtToIRSB(cdgs.sbOut, sbIn->stmts[i]);
		i++;
	}

	// Get the first statement, and origAddr from it
	CGD_ASSERT(sbIn->stmts_used > 0);
	CGD_ASSERT(i < sbIn->stmts_used);
	st = sbIn->stmts[i];
	CGD_ASSERT(Ist_IMark == st->tag);

	origAddr = st->Ist.IMark.addr + st->Ist.IMark.delta;
	CGD_ASSERT(origAddr == st->Ist.IMark.addr + st->Ist.IMark.delta); // XXX: check no overflow

	/* Get BB struct (creating if necessary).
	 * JS: The hash table is keyed with orig_addr_noredir -- important!
	 * JW: Why? If it is because of different chasing of the redirection,
	 *     this is not needed, as chasing is switched off in CFGgrind.
	 */
	cdgs.bb = CGD_(get_bb)(origAddr, sbIn, &(cdgs.seen_before));

	addBBSetupCall(&cdgs);

	// Set up running state
	cdgs.ii_index = 0;
	cdgs.ig_index = 0;
	cdgs.instr_offset = 0;

	for (/*use current i*/; i < sbIn->stmts_used; i++) {
		st = sbIn->stmts[i];
		CGD_ASSERT(isFlatIRStmt(st));

		++instr_stmt_count;

		switch (st->tag) {
		case Ist_NoOp:
		case Ist_AbiHint:
		case Ist_PutI:
		case Ist_MBE:
			break;

		case Ist_IMark: {
			Addr cia = st->Ist.IMark.addr + st->Ist.IMark.delta;
			UInt isize = st->Ist.IMark.len;
			CGD_ASSERT(cdgs.instr_offset == cia - origAddr);
			// If Vex fails to decode an instruction, the size will be zero.
			// Pretend otherwise.
			if (isize == 0)
				isize = VG_MIN_INSTR_SZB;

			// Sanity-check size.
			tl_assert(
					(VG_MIN_INSTR_SZB <= isize && isize <= VG_MAX_INSTR_SZB)
							|| VG_CLREQ_SZB == isize);

			// The next group must always come before next_InstrInfo.
			if (nextGroup) {
				// Create the group.
				curr_group = next_InstrGroupInfo(&cdgs);

				// The group is not new anymore.
				nextGroup = False;
			}

			// Init the inode, record it as the current one.
			// Subsequent Dr/Dw/Dm events from the same instruction will
			// also use it.
			curr_inode = next_InstrInfo(&cdgs, isize);

			// Account the stats for this instruction if it is the first time.
			tl_assert(curr_group != 0);
			if (!cdgs.seen_before) {
				curr_group->group_size += curr_inode->instr_size;
				curr_group->instr_count++;
				curr_group->bb_info.last_instr = (cdgs.ii_index - 1);
			}

			instr_stmt_count = 0;

			break;
		}

		case Ist_Put:
		case Ist_WrTmp:
		case Ist_Store:
		case Ist_StoreG:
		case Ist_LoadG:
		case Ist_Dirty:
		case Ist_CAS:
		case Ist_LLSC:
			break;
		case Ist_Exit: {
			Bool guest_exit, inverted;

			/* VEX code generation sometimes inverts conditional branches.
			 * As cfggrind counts (conditional) jumps, it has to correct
			 * inversions. The heuristic is the following:
			 * (1) cfggrind switches off SB chasing and unrolling, and
			 *     therefore it assumes that a candidate for inversion only is
			 *     the last conditional branch in an SB.
			 * (2) inversion is assumed if the branch jumps to the address of
			 *     the next guest instruction in memory.
			 * This heuristic is precalculated in CGD_(collectBlockInfo)().
			 *
			 * Branching behavior is also used for branch prediction. Note that
			 * above heuristic is different from what Cachegrind does.
			 * Cachegrind uses (2) for all branches.
			 */
			if (cJumps + 1 == cdgs.bb->cjmp_count)
				inverted = cdgs.bb->cjmp_inverted;
			else
				inverted = False;

			// call branch predictor only if this is a branch in guest code
			guest_exit = (st->Ist.Exit.jk == Ijk_Boring)
					|| (st->Ist.Exit.jk == Ijk_Call)
					|| (st->Ist.Exit.jk == Ijk_Ret);

			if (guest_exit) {
				/* Stuff to widen the guard expression to a host word, so
				 we can pass it to the branch predictor simulation
				 functions easily. */
				IRType tyW = hWordTy;
				IROp widen = tyW == Ity_I32 ? Iop_1Uto32 : Iop_1Uto64;
				IROp opXOR = tyW == Ity_I32 ? Iop_Xor32 : Iop_Xor64;
				IRTemp guard1 = newIRTemp(cdgs.sbOut->tyenv, Ity_I1);
				IRTemp guardW = newIRTemp(cdgs.sbOut->tyenv, tyW);
				IRTemp guard = newIRTemp(cdgs.sbOut->tyenv, tyW);
				IRExpr* one =
						tyW == Ity_I32 ?
								IRExpr_Const(IRConst_U32(1)) :
								IRExpr_Const(IRConst_U64(1));

				/* Widen the guard expression. */
				addStmtToIRSB(cdgs.sbOut,
						IRStmt_WrTmp(guard1, st->Ist.Exit.guard));
				addStmtToIRSB(cdgs.sbOut,
						IRStmt_WrTmp(guardW,
								IRExpr_Unop(widen, IRExpr_RdTmp(guard1))));
				/* If the exit is inverted, invert the sense of the guard. */
				addStmtToIRSB(cdgs.sbOut,
						IRStmt_WrTmp(guard,
								inverted ?
										IRExpr_Binop(opXOR,
												IRExpr_RdTmp(guardW), one) :
										IRExpr_RdTmp(guardW)));
			}

			CGD_ASSERT(cdgs.ii_index > 0);
			if (!cdgs.seen_before) {
				Addr dst;
				BBJumpKind jk;

				dst = 0;
				if (st->Ist.Exit.jk == Ijk_Call) {
					jk = bjk_Call;
				} else if (st->Ist.Exit.jk == Ijk_Ret) {
					jk = bjk_Return;
				} else {
					dst = IRConst2Addr(st->Ist.Exit.dst);
					if (dst == origAddr + curr_inode->instr_offset
									+ curr_inode->instr_size)
						jk = bjk_None;
					else
						jk = bjk_Jump;
				}

				cdgs.bb->jmp[cJumps].instr = cdgs.ii_index - 1;
				cdgs.bb->jmp[cJumps].group = cdgs.ig_index - 1;
				cdgs.bb->jmp[cJumps].jmpkind = jk;
				cdgs.bb->jmp[cJumps].dst = dst;
				// Exit jumps are never indirect.
				cdgs.bb->jmp[cJumps].indirect = False;
			}

			/* Update global variable jmps_passed before the jump
			 * A correction is needed if VEX inverted the last jump condition
			 */
			UInt val = inverted ? cJumps + 1 : cJumps;
			addConstMemStoreStmt(cdgs.sbOut,
					(UWord) &CGD_(current_state).jmps_passed, val, hWordTy);
			cJumps++;

			// Mark the new instruction as the beginning of a new group of instructions.
			nextGroup = True;

			break;
		}

		default:
			tl_assert(0);
			break;
		}

		/* Copy the original statement */
		addStmtToIRSB(cdgs.sbOut, st);

		CGD_DEBUGIF(5) {
			VG_(printf)("   pass  ");
			ppIRStmt(st);
			VG_(printf)("\n");
		}
	}

	/* Deal with branches to unknown destinations.  Except ignore ones
	 which are function returns as we assume the return stack
	 predictor never mispredicts. */
	if ((sbIn->jumpkind == Ijk_Boring) || (sbIn->jumpkind == Ijk_Call)) {
		if (0) {
			ppIRExpr(sbIn->next);
			VG_(printf)("\n");
		}
		switch (sbIn->next->tag) {
		case Iex_Const:
		case Iex_RdTmp:
			break;
		default:
			/* shouldn't happen - if the incoming IR is properly
			 flattened, should only have tmp and const cases to
			 consider. */
			tl_assert(0);
		}
	}

	/* Update global variable jmps_passed at end of SB.
	 * As CGD_(current_state).jmps_passed is reset to 0 in setup_bb,
	 * this can be omitted if there is no conditional jump in this SB.
	 * A correction is needed if VEX inverted the last jump condition
	 */
	if (cJumps > 0) {
		UInt jmps_passed = cJumps;
		if (cdgs.bb->cjmp_inverted)
			jmps_passed--;
		addConstMemStoreStmt(cdgs.sbOut,
				(UWord) &CGD_(current_state).jmps_passed, jmps_passed, hWordTy);
	}
	CGD_ASSERT(cdgs.bb->cjmp_count == cJumps);
	CGD_ASSERT(cdgs.bb->instr_count == cdgs.ii_index);
	CGD_ASSERT(cdgs.bb->groups_count == cdgs.ig_index);

	/* Info for final exit from BB */
	{
		BBJumpKind jk;
		Addr dst;
		Bool indirect;

		dst = 0;
		indirect = sbIn->next->tag != Iex_Const;

		if (sbIn->jumpkind == Ijk_Call) {
			jk = bjk_Call;
		} else if (sbIn->jumpkind == Ijk_Ret) {
			jk = bjk_Return;
		} else {
			jk = bjk_Jump;

			// Find the destination address only if the jump is direct.
			if (!indirect) {
				dst = IRConst2Addr(sbIn->next->Iex.Const.con);

				// The jump is actually a fallthrough if:
				// (1) the destination address matches the next instruction;
				// (2) the last instruction has statements (thus, it is not a
				// jump instruction)
				if ((dst == origAddr + cdgs.instr_offset) && instr_stmt_count > 0)
					jk = bjk_None;
			}
		}

		cdgs.bb->jmp[cJumps].jmpkind = jk;
		/* Instruction index of the call/ret at BB end
		 * (it is wrong for fall-through, but does not matter) */
		cdgs.bb->jmp[cJumps].instr = cdgs.ii_index - 1;
		cdgs.bb->jmp[cJumps].group = cdgs.ig_index - 1;
		cdgs.bb->jmp[cJumps].dst = dst;
		cdgs.bb->jmp[cJumps].indirect = indirect;
	}

	/* swap information of last exit with final exit if inverted */
	if (cdgs.bb->cjmp_inverted) {
		CJmpInfo tmp;

		tmp = cdgs.bb->jmp[cJumps];
		cdgs.bb->jmp[cJumps] = cdgs.bb->jmp[cJumps - 1];
		cdgs.bb->jmp[cJumps - 1] = tmp;
	}

	if (cdgs.seen_before) {
		CGD_ASSERT(cdgs.bb->instr_len == cdgs.instr_offset);
	} else {
		cdgs.bb->instr_len = cdgs.instr_offset;
	}

	CGD_DEBUG(3, "- instrument(BB %#lx): byteLen %u, CJumps %u\n",
			origAddr, cdgs.bb->instr_len, cdgs.bb->cjmp_count);
	if (cJumps > 0) {
		CGD_DEBUG(3, "                     [ ");
		for (i = 0; i < cJumps; i++)
			CGD_DEBUG(3, "%u ", cdgs.bb->jmp[i].instr);
		CGD_DEBUG(3, "], last inverted: %s \n",
				cdgs.bb->cjmp_inverted ? "yes" : "no");
	}

	return cdgs.sbOut;
}

/*--------------------------------------------------------------------*/
/*--- Discarding BB info                                           ---*/
/*--------------------------------------------------------------------*/

// Called when a translation is removed from the translation cache for
// any reason at all: to free up space, because the guest code was
// unmapped or modified, or for any arbitrary reason.
static
void cdg_discard_superblock_info(Addr orig_addr, VexGuestExtents vge) {
	tl_assert(vge.n_used > 0);

	if (0)
		VG_(printf)("discard_superblock_info: %p, %p, %llu\n",
				(void*) orig_addr, (void*) vge.base[0], (ULong) vge.len[0]);

	// Get BB info, remove from table, free BB info.  Simple!
	// When created, the BB is keyed by the first instruction address,
	// (not orig_addr, but eventually redirected address). Thus, we
	// use the first instruction address in vge.
	CGD_(delete_bb)(vge.base[0]);
}

static
void unwind_thread(thread_info* t) {
	/* unwind signal handlers */
	while (CGD_(current_state).sig != 0)
		CGD_(post_signal)(CGD_(current_tid), CGD_(current_state).sig);

	/* unwind regular call stack */
	while (CGD_(current_call_stack).sp > 0)
		CGD_(pop_call_stack)(True);

	// Set the last working instructions to its exit node.
	CGD_(cfgnode_set_halt)(CGD_(current_state).cfg, CGD_(current_state).working);

	/* reset context and function stack for context generation */
	CGD_(init_exec_state)(&CGD_(current_state));
}

static
void dump_memory_mappings(const HChar* filename) {
	const DebugInfo* di;
	VgFile* fp;

	fp = VG_(fopen)(filename, VKI_O_WRONLY|VKI_O_TRUNC, 0);
	if (fp == NULL) {
		fp = VG_(fopen)(filename, VKI_O_CREAT|VKI_O_WRONLY,
				VKI_S_IRUSR|VKI_S_IWUSR);
	}
	CGD_ASSERT(fp != 0);

	for (di = VG_(next_DebugInfo)(0); di; di = VG_(next_DebugInfo)(di)) {
		Addr addr;
		SizeT size;

		addr = VG_(DebugInfo_get_text_avma)(di);
		if (!addr)
			continue;

		size = VG_(DebugInfo_get_text_size)(di);
		CGD_ASSERT(size > 0);

		VG_(fprintf)(fp, "%s:0x%lx:%lu\n",
			VG_(DebugInfo_get_filename)(di), addr, size);
	}

	VG_(fclose)(fp);
}


static
void cdg_print_stats(void) {
	int BB_lookups =
	CGD_(stat).full_debug_BBs +
	CGD_(stat).fn_name_debug_BBs +
	CGD_(stat).file_line_debug_BBs +
	CGD_(stat).no_debug_BBs;

	/* Hash table stats */
	VG_(message)(Vg_DebugMsg, "Distinct objects:   %d\n",
	CGD_(stat).distinct_objs);
	VG_(message)(Vg_DebugMsg, "Distinct files:     %d\n",
	CGD_(stat).distinct_files);
	VG_(message)(Vg_DebugMsg, "Distinct fns:       %d\n",
	CGD_(stat).distinct_fns);
	VG_(message)(Vg_DebugMsg, "Distinct BBs:       %d\n",
	CGD_(stat).distinct_bbs);
	VG_(message)(Vg_DebugMsg, "BB lookups:         %d\n", BB_lookups);
	if (BB_lookups > 0) {
		VG_(message)(Vg_DebugMsg, "With full      debug info:%3d%% (%d)\n",
		CGD_(stat).full_debug_BBs * 100 / BB_lookups,
		CGD_(stat).full_debug_BBs);
		VG_(message)(Vg_DebugMsg, "With file/line debug info:%3d%% (%d)\n",
		CGD_(stat).file_line_debug_BBs * 100 / BB_lookups,
		CGD_(stat).file_line_debug_BBs);
		VG_(message)(Vg_DebugMsg, "With fn name   debug info:%3d%% (%d)\n",
		CGD_(stat).fn_name_debug_BBs * 100 / BB_lookups,
		CGD_(stat).fn_name_debug_BBs);
		VG_(message)(Vg_DebugMsg, "With no        debug info:%3d%% (%d)\n",
		CGD_(stat).no_debug_BBs * 100 / BB_lookups,
		CGD_(stat).no_debug_BBs);
	}
	VG_(message)(Vg_DebugMsg, "BBs Retranslated:   %d\n",
	CGD_(stat).bb_retranslations);
	VG_(message)(Vg_DebugMsg, "Distinct instrs:    %d\n",
	CGD_(stat).distinct_instrs);
	VG_(message)(Vg_DebugMsg, "Distinct groups:    %d\n",
	CGD_(stat).distinct_groups);
	VG_(message)(Vg_DebugMsg, "Distinct CFGs:      %d\n",
	CGD_(stat).distinct_cfgs);
	VG_(message)(Vg_DebugMsg, "Distinct CFG nodes: %d\n",
	CGD_(stat).distinct_cfg_nodes);
	VG_(message)(Vg_DebugMsg, "BBs Executed:       %llu\n",
	CGD_(stat).bb_executions);
}

static
void finish(void) {
	HChar* filename;
	CGD_DEBUG(0, "finish()\n");

	/* pop all remaining items from CallStack for correct sum
	 */
	CGD_(forall_threads)(unwind_thread);

#if ENABLE_PROFILING && CFG_NODE_CACHE_SIZE > 0
	CGD_(forall_cfg)(CGD_(cfg_flush_all_counts));
#endif

	// Fix CFG if possible.
	CGD_(forall_cfg)(CGD_(fix_cfg));

	// Always check the CFGs.
	CGD_(forall_cfg)(CGD_(check_cfg));

	if (CGD_(clo).cfg_outfile) {
		filename = VG_(expand_file_name)("--cfg-outfile",
						CGD_(clo).cfg_outfile);
		CGD_(write_cfgs)(filename);
		VG_(free)(filename);
	}

	if (CGD_(clo).mem_mappings) {
		filename = VG_(expand_file_name)("--mem-mappings",
						CGD_(clo).mem_mappings);
		dump_memory_mappings(filename);
		VG_(free)(filename);
	}

	// Dump the CFG dot files.
	CGD_(forall_cfg)(CGD_(dump_cfg));

	CGD_(destroy_threads)();
	CGD_(destroy_instrs_pool)();
	CGD_(destroy_cfg_hash)();
	CGD_(destroy_bb_hash)();
	CGD_(destroy_obj_table)();

	if (VG_(clo_verbosity) == 0)
		return;

	if (VG_(clo_stats)) {
		VG_(message)(Vg_DebugMsg, "\n");
		cdg_print_stats();
		VG_(message)(Vg_DebugMsg, "\n");
	}
}

void CGD_(fini)(Int exitcode) {
	finish();
}

/*--------------------------------------------------------------------*/
/*--- Setup                                                        ---*/
/*--------------------------------------------------------------------*/

static void cdg_start_client_code_callback(ThreadId tid, ULong blocks_done) {
	static ULong last_blocks_done = 0;

	if (0)
		VG_(printf)("%d R %llu\n", (Int) tid, blocks_done);

	/* throttle calls to CGD_(run_thread) by number of BBs executed */
	if (blocks_done - last_blocks_done < 5000)
		return;
	last_blocks_done = blocks_done;

	CGD_(run_thread)(tid);
}

static
void CGD_(post_clo_init)(void) {
	if (VG_(clo_vex_control).iropt_register_updates_default
			!= VexRegUpdSpAtMemAccess) {
		CGD_DEBUG(1, " Using user specified value for "
				"--vex-iropt-register-updates\n");
	} else {
		CGD_DEBUG(1, " Using default --vex-iropt-register-updates="
				"sp-at-mem-access\n");
	}

	if (VG_(clo_px_file_backed) != VexRegUpdSpAtMemAccess) {
		CGD_DEBUG(1, " Using user specified value for "
				"--px-file-backed\n");
	} else {
		CGD_DEBUG(1, " Using default --px-file-backed="
				"sp-at-mem-access\n");
	}

	if (VG_(clo_vex_control).iropt_unroll_thresh != 0) {
		VG_(message)(Vg_UserMsg,
				"cfggrind only works with --vex-iropt-unroll-thresh=0\n"
						"=> resetting it back to 0\n");
		VG_(clo_vex_control).iropt_unroll_thresh = 0; // cannot be overridden.
	}
	if (VG_(clo_vex_control).guest_chase) {
		VG_(message)(Vg_UserMsg,
				"cfggrind only works with --vex-guest-chase=no\n"
						"=> resetting it back to 'no'\n");
		VG_(clo_vex_control).guest_chase = False; // cannot be overridden.
	}

	CGD_(init_statistics)(&CGD_(stat));

	/* initialize hash tables */
	CGD_(init_obj_table)();
	CGD_(init_bb_hash)();
	CGD_(init_cfg_hash)();

	CGD_(init_instrs_pool)();

	// read the cfg from file if option is present.
	if (CGD_(clo).cfg_infile) {
		Int fd = VG_(fd_open)(CGD_(clo).cfg_infile, VKI_O_RDONLY, 0);
		if (fd > 0) {
			CGD_(read_cfgs)(fd);
			VG_(close)(fd);
		} else {
			tl_assert(CGD_(clo).ignore_failed);
		}
	}

	CGD_(init_threads)();
	CGD_(run_thread)(1);
}

static
void CGD_(pre_clo_init)(void) {
	VG_(details_name)("cfggrind");
	VG_(details_version)(NULL);
	VG_(details_description)("a dynamic control flow graph (CFG) reconstruction tool");
	VG_(details_copyright_author)("Copyright (C) 2023, and GNU GPL'd, "
			"by Andrei Rimsa with code from Callgrind");
	VG_(details_bug_reports_to)(VG_BUGS_TO);
	VG_(details_avg_translation_sizeB)(500);

	VG_(clo_vex_control).iropt_register_updates_default =
	VG_(clo_px_file_backed) = VexRegUpdSpAtMemAccess; // overridable by the user.

	VG_(clo_vex_control).iropt_unroll_thresh = 0;   // cannot be overridden.
	VG_(clo_vex_control).guest_chase = False;    // cannot be overridden.

	VG_(basic_tool_funcs)(CGD_(post_clo_init), CGD_(instrument),
			CGD_(fini));

	VG_(needs_superblock_discards)(cdg_discard_superblock_info);

	VG_(needs_command_line_options)(CGD_(process_cmd_line_option),
			CGD_(print_usage), CGD_(print_debug_usage));

	VG_(needs_print_stats)(cdg_print_stats);

	VG_(track_start_client_code)(&cdg_start_client_code_callback);
	VG_(track_pre_deliver_signal)(&CGD_(pre_signal));
	VG_(track_post_deliver_signal)(&CGD_(post_signal));

	CGD_(set_clo_defaults)();
}

VG_DETERMINE_INTERFACE_VERSION(CGD_(pre_clo_init))

/*--------------------------------------------------------------------*/
/*--- end                                                   main.c ---*/
/*--------------------------------------------------------------------*/
