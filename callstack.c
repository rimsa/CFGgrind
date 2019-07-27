/*--------------------------------------------------------------------*/
/*--- Callgrind                                                    ---*/
/*---                                               ct_callstack.c ---*/
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

/*------------------------------------------------------------*/
/*--- Call stack, operations                               ---*/
/*------------------------------------------------------------*/

/* Stack of current thread. Gets initialized when switching to 1st thread.
 *
 * The artificial call stack is an array of call_entry's, representing
 * stack frames of the executing program. 
 * Array call_stack and call_stack_esp have same size and grow on demand.
 * Array call_stack_esp holds SPs of corresponding stack frames.
 *
 */

#define N_CALL_STACK_INITIAL_ENTRIES 500

call_stack LPG_(current_call_stack);

void LPG_(init_call_stack)(call_stack* s)
{
  Int i;

  LPG_ASSERT(s != 0);

  s->size = N_CALL_STACK_INITIAL_ENTRIES;   
  s->entry = (call_entry*) LPG_MALLOC("cl.callstack.ics.1",
                                      s->size * sizeof(call_entry));
  s->sp = 0;
  s->entry[0].cxt = 0; /* for assertion in push_cxt() */

  for(i=0; i<s->size; i++) {
	  s->entry[i].cfg = 0;
	  s->entry[i].dangling = 0;
  }
}

void LPG_(destroy_call_stack)(call_stack* s) {
	LPG_ASSERT(s != 0);

	LPG_FREE(s->entry);
}

call_entry* LPG_(get_call_entry)(Int sp)
{
  LPG_ASSERT(sp <= LPG_(current_call_stack).sp);
  return &(LPG_(current_call_stack).entry[sp]);
}

void LPG_(copy_current_call_stack)(call_stack* dst)
{
  LPG_ASSERT(dst != 0);

  dst->size  = LPG_(current_call_stack).size;
  dst->entry = LPG_(current_call_stack).entry;
  dst->sp    = LPG_(current_call_stack).sp;
}

void LPG_(set_current_call_stack)(call_stack* s)
{
  LPG_ASSERT(s != 0);

  LPG_(current_call_stack).size  = s->size;
  LPG_(current_call_stack).entry = s->entry;
  LPG_(current_call_stack).sp    = s->sp;
}


static __inline__
void ensure_stack_size(Int i)
{
  Int oldsize;
  call_stack *cs = &LPG_(current_call_stack);

  if (i < cs->size) return;

  oldsize = cs->size;
  cs->size *= 2;
  while (i > cs->size) cs->size *= 2;

  cs->entry = (call_entry*) LPG_REALLOC("cl.callstack.ess.1",
		  	  	  cs->entry, cs->size * sizeof(call_entry));

  for(i=oldsize; i<cs->size; i++) {
    cs->entry[i].cfg = 0;
    cs->entry[i].dangling = 0;
  }

  LPG_(stat).call_stack_resizes++;
 
  LPG_DEBUGIF(2)
    VG_(printf)("        call stack enlarged to %u entries\n",
		LPG_(current_call_stack).size);
}

/* Push call on call stack.
 *
 * Increment the usage count for the function called.
 * A jump from <from> to <to>, with <sp>.
 */
void LPG_(push_call_stack)(BBCC* from, UInt jmp, BBCC* to, Addr sp)
{
    call_entry* current_entry;
    Addr ret_addr;
    CFG* callee;

    /* Ensure a call stack of size <current_sp>+1.
     * The +1 is needed as push_cxt will store the
     * context at [current_sp]
     */
    ensure_stack_size(LPG_(current_call_stack).sp +1);
    current_entry = &(LPG_(current_call_stack).entry[LPG_(current_call_stack).sp]);

	/* As push_cxt() has to be called before push_call_stack,
	 * the old context should already be saved on the stack */
	LPG_ASSERT(current_entry->cxt != 0);

    /* return address is only is useful with a real call;
     * used to detect RET w/o CALL */
    if (from->bb->jmp[jmp].jmpkind == jk_Call) {
		UInt instr = from->bb->jmp[jmp].instr;
		ret_addr = bb_addr(from->bb) +
		from->bb->instr[instr].instr_offset +
		from->bb->instr[instr].instr_size;
    } else {
    		ret_addr = 0;
    }

    callee = LPG_(get_cfg)(to->bb->groups[0].group_addr);

	// Let's update the fdesc if it is our first real call to it.
	if (!LPG_(cfg_fdesc)(callee))
		LPG_(cfg_build_fdesc)(callee);

#ifdef LPG_ENABLE_PATH_CACHE
	if (!LPG_(current_state).dangling->cache ||
		LPG_(current_state).dangling->cache->call.cfg != callee ||
		LPG_(current_state).dangling->cache->call.indirect != from->bb->jmp[jmp].indirect) {
		LPG_(cfgnode_set_call)(LPG_(current_state).cfg, LPG_(current_state).dangling,
				callee, from->bb->jmp[jmp].indirect);
	}
#else
	LPG_(cfgnode_set_call)(LPG_(current_state).cfg, LPG_(current_state).dangling,
			callee, from->bb->jmp[jmp].indirect);
#endif

    /* put jcc on call stack */
    current_entry->sp = sp;
    current_entry->ret_addr = ret_addr;
    current_entry->cfg = LPG_(current_state).cfg;
    current_entry->dangling = LPG_(current_state).dangling;

    LPG_(current_call_stack).sp++;

    /* To allow for above assertion we set context of next frame to 0 */
    LPG_ASSERT(LPG_(current_call_stack).sp < LPG_(current_call_stack).size);
    current_entry++;

    current_entry->cxt = 0;
    current_entry->cfg = 0;
    current_entry->dangling = 0;

	// If the parent cfg is inside main, then this CFG is inside main as well.
	if (!LPG_(cfg_is_inside_main)(callee))
		LPG_(cfg_set_inside_main)(callee, LPG_(cfg_is_inside_main)(LPG_(current_state).cfg));

    LPG_(current_state).cfg = callee;
    LPG_(current_state).dangling = 0;
}


/* Pop call stack and update inclusive sums.
 * Returns modified fcc.
 *
 * If the JCC becomes inactive, call entries are freed if possible
 */
void LPG_(pop_call_stack)(Bool halt) {
    call_entry* lower_entry;

    if (LPG_(current_state).sig > 0) {
		/* Check if we leave a signal handler; this can happen when
		 * calling longjmp() in the handler */
		LPG_(run_post_signal_on_call_stack_bottom)();
    }

    lower_entry =
    		&(LPG_(current_call_stack).entry[LPG_(current_call_stack).sp-1]);

    LPG_DEBUG(4,"+ pop_call_stack: frame %d\n",
		LPG_(current_call_stack).sp);

    /* restore context */
    LPG_(current_state).cxt = lower_entry->cxt;
    LPG_(current_fn_stack).top = LPG_(current_fn_stack).bottom + lower_entry->fn_sp;
    LPG_ASSERT(LPG_(current_state).cxt != 0);

    /* To allow for an assertion in push_call_stack() */
    lower_entry->cxt = 0;

	if (halt) {
		LPG_(cfgnode_set_halt)(LPG_(current_state).cfg, &(LPG_(current_state).dangling));
	} else {
#ifdef LPG_ENABLE_PATH_CACHE
		if (LPG_(current_state).dangling->cache && LPG_(current_state).dangling->cache->exit)
			LPG_(current_state).dangling = 0;
		else
			LPG_(cfgnode_set_exit)(LPG_(current_state).cfg, &(LPG_(current_state).dangling));
#else
		LPG_(cfgnode_set_exit)(LPG_(current_state).cfg, &(LPG_(current_state).dangling));
#endif
    }

	LPG_(current_state).cfg = lower_entry->cfg;
	LPG_(current_state).dangling = lower_entry->dangling;

	lower_entry->cfg = 0;
	lower_entry->dangling = 0;

    LPG_(current_call_stack).sp--;
}


/* Unwind enough CallStack items to sync with current stack pointer.
 * Returns the number of stack frames unwinded.
 */
Int LPG_(unwind_call_stack)(Addr sp, Int minpops)
{
    Int csp;
    Int unwind_count = 0;
    LPG_DEBUG(4,"+ unwind_call_stack(sp %#lx, minpops %d): frame %d\n",
	      sp, minpops, LPG_(current_call_stack).sp);

    /* We pop old stack frames.
     * For a call, be p the stack address with return address.
     *  - call_stack_esp[] has SP after the CALL: p-4
     *  - current sp is after a RET: >= p
     */
    
    while( (csp=LPG_(current_call_stack).sp) >0) {
	call_entry* top_ce = &(LPG_(current_call_stack).entry[csp-1]);

	if ((top_ce->sp < sp) ||
	    ((top_ce->sp == sp) && minpops>0)) {

	    minpops--;
	    unwind_count++;
	    LPG_(pop_call_stack)(False);
	    csp=LPG_(current_call_stack).sp;
	    continue;
	}
	break;
    }

    LPG_DEBUG(4,"- unwind_call_stack\n");
    return unwind_count;
}
