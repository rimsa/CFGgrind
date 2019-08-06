/*--------------------------------------------------------------------*/
/*--- CFGgrind                                                     ---*/
/*---                                                  callstack.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of CFGgrind, a dynamic control flow graph (CFG)
   reconstruction tool.

   Copyright (C) 2019, Andrei Rimsa (andrei@cefetmg.br)

   This tool is derived from and contains lot of code from Callgrind
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

call_stack CGD_(current_call_stack);

void CGD_(init_call_stack)(call_stack* s)
{
  Int i;

  CGD_ASSERT(s != 0);

  s->size = N_CALL_STACK_INITIAL_ENTRIES;   
  s->entry = (call_entry*) CGD_MALLOC("cgd.callstack.ics.1",
                                      s->size * sizeof(call_entry));
  s->sp = 0;

  for(i=0; i<s->size; i++) {
	  s->entry[i].cfg = 0;
	  s->entry[i].dangling = 0;
  }
}

void CGD_(destroy_call_stack)(call_stack* s) {
	CGD_ASSERT(s != 0);

	CGD_FREE(s->entry);
}

call_entry* CGD_(get_call_entry)(Int sp)
{
  CGD_ASSERT(sp <= CGD_(current_call_stack).sp);
  return &(CGD_(current_call_stack).entry[sp]);
}

void CGD_(copy_current_call_stack)(call_stack* dst)
{
  CGD_ASSERT(dst != 0);

  dst->size  = CGD_(current_call_stack).size;
  dst->entry = CGD_(current_call_stack).entry;
  dst->sp    = CGD_(current_call_stack).sp;
}

void CGD_(set_current_call_stack)(call_stack* s)
{
  CGD_ASSERT(s != 0);

  CGD_(current_call_stack).size  = s->size;
  CGD_(current_call_stack).entry = s->entry;
  CGD_(current_call_stack).sp    = s->sp;
}


static __inline__
void ensure_stack_size(Int i)
{
  Int oldsize;
  call_stack *cs = &CGD_(current_call_stack);

  if (i < cs->size) return;

  oldsize = cs->size;
  cs->size *= 2;
  while (i > cs->size) cs->size *= 2;

  cs->entry = (call_entry*) CGD_REALLOC("cgd.callstack.ess.1",
		  	  	  cs->entry, cs->size * sizeof(call_entry));

  for(i=oldsize; i<cs->size; i++) {
    cs->entry[i].cfg = 0;
    cs->entry[i].dangling = 0;
  }

  CGD_(stat).call_stack_resizes++;
 
  CGD_DEBUGIF(2)
    VG_(printf)("        call stack enlarged to %u entries\n",
		CGD_(current_call_stack).size);
}

/* Push call on call stack.
 *
 * Increment the usage count for the function called.
 * A jump from <from> to <to>, with <sp>.
 */
void CGD_(push_call_stack)(BB* from, UInt jmp, BB* to, Addr sp)
{
    call_entry* current_entry;
    Addr ret_addr;
    CFG* callee;
#if CFG_NODE_CACHE_SIZE > 0
    CfgNodeCallCache* callCache;
#endif

    /* Ensure a call stack of size <current_sp>+1.
     */
    ensure_stack_size(CGD_(current_call_stack).sp +1);
    current_entry = &(CGD_(current_call_stack).entry[CGD_(current_call_stack).sp]);

    /* return address is only is useful with a real call;
     * used to detect RET w/o CALL */
    if (from->jmp[jmp].jmpkind == bjk_Call) {
		UInt instr = from->jmp[jmp].instr;
		ret_addr = bb_addr(from) +
						from->instr[instr].instr_offset +
						from->instr[instr].instr_size;
    } else {
    		ret_addr = 0;
    }

    callee = CGD_(get_cfg)(to->groups[0].group_addr);

	// Let's update the fdesc if it is our first real call to it.
	if (!CGD_(cfg_fdesc)(callee))
		CGD_(cfg_build_fdesc)(callee);

#if CFG_NODE_CACHE_SIZE > 0
	callCache = CGD_(current_state).dangling->cache.call ?
			&(CGD_(current_state).dangling->cache.call[CFG_NODE_CACHE_INDEX(callee->addr)]) : 0;
	if (!callCache ||
			callCache->addr != callee->addr ||
			callCache->indirect != from->jmp[jmp].indirect)
#endif
		CGD_(cfgnode_set_call)(CGD_(current_state).cfg, CGD_(current_state).dangling,
				callee, from->jmp[jmp].indirect);

    /* put jcc on call stack */
    current_entry->sp = sp;
    current_entry->ret_addr = ret_addr;
    current_entry->cfg = CGD_(current_state).cfg;
    current_entry->dangling = CGD_(current_state).dangling;

    CGD_(current_call_stack).sp++;

    /* To allow for above assertion we set context of next frame to 0 */
    CGD_ASSERT(CGD_(current_call_stack).sp < CGD_(current_call_stack).size);
    current_entry++;

    current_entry->cfg = 0;
    current_entry->dangling = 0;

	// If the parent cfg is inside main, then this CFG is inside main as well.
	if (!CGD_(cfg_is_inside_main)(callee))
		CGD_(cfg_set_inside_main)(callee, CGD_(cfg_is_inside_main)(CGD_(current_state).cfg));

    CGD_(current_state).cfg = callee;
    CGD_(current_state).dangling = CGD_(cfg_entry_node)(callee);
}


/* Pop call stack and update inclusive sums.
 * Returns modified fcc.
 *
 * If the JCC becomes inactive, call entries are freed if possible
 */
void CGD_(pop_call_stack)(Bool halt) {
    call_entry* lower_entry;

    if (CGD_(current_state).sig > 0) {
		/* Check if we leave a signal handler; this can happen when
		 * calling longjmp() in the handler */
		CGD_(run_post_signal_on_call_stack_bottom)();
    }

    lower_entry =
    		&(CGD_(current_call_stack).entry[CGD_(current_call_stack).sp-1]);

    CGD_DEBUG(4,"+ pop_call_stack: frame %d\n",
		CGD_(current_call_stack).sp);

	if (halt) {
		CGD_(cfgnode_set_halt)(CGD_(current_state).cfg, CGD_(current_state).dangling);
	} else {
#if CFG_NODE_CACHE_SIZE > 0
		if (!CGD_(current_state).dangling->cache.exit)
#endif
			CGD_(cfgnode_set_exit)(CGD_(current_state).cfg, CGD_(current_state).dangling);
    }

	CGD_(current_state).cfg = lower_entry->cfg;
	CGD_(current_state).dangling = lower_entry->dangling;

	lower_entry->cfg = 0;
	lower_entry->dangling = 0;

    CGD_(current_call_stack).sp--;
}


/* Unwind enough CallStack items to sync with current stack pointer.
 * Returns the number of stack frames unwinded.
 */
Int CGD_(unwind_call_stack)(Addr sp, Int minpops)
{
    Int csp;
    Int unwind_count = 0;
    CGD_DEBUG(4,"+ unwind_call_stack(sp %#lx, minpops %d): frame %d\n",
	      sp, minpops, CGD_(current_call_stack).sp);

    /* We pop old stack frames.
     * For a call, be p the stack address with return address.
     *  - call_stack_esp[] has SP after the CALL: p-4
     *  - current sp is after a RET: >= p
     */
    
    while( (csp=CGD_(current_call_stack).sp) >0) {
	call_entry* top_ce = &(CGD_(current_call_stack).entry[csp-1]);

	if ((top_ce->sp < sp) ||
	    ((top_ce->sp == sp) && minpops>0)) {

	    minpops--;
	    unwind_count++;
	    CGD_(pop_call_stack)(False);
	    csp=CGD_(current_call_stack).sp;
	    continue;
	}
	break;
    }

    CGD_DEBUG(4,"- unwind_call_stack\n");
    return unwind_count;
}
