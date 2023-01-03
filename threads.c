/*--------------------------------------------------------------------*/
/*--- CFGgrind                                                     ---*/
/*---                                                    threads.c ---*/
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

#include "pub_tool_threadstate.h"

/* forward decls */
static exec_state* exec_state_save(void);
static exec_state* exec_state_restore(void);
static exec_state* push_exec_state(int);
static exec_state* top_exec_state(void);

static exec_stack current_states;


/*------------------------------------------------------------*/
/*--- Support for multi-threading                          ---*/
/*------------------------------------------------------------*/


/*
 * For Valgrind, MT is cooperative (no preemting in our code),
 * so we don't need locks...
 *
 * Per-thread data:
 *  - call stack
 *  - call hash
 *  - event counters: last, current
 *
 * Even when ignoring MT, we need this functions to set up some
 * datastructures for the process (= Thread 1).
 */

/* current running thread */
ThreadId CGD_(current_tid);

static thread_info** threads;

thread_info** CGD_(get_threads)()
{
  return threads;
}

thread_info* CGD_(get_current_thread)()
{
  return threads[CGD_(current_tid)];
}

void CGD_(init_threads)() {
	UInt i;

	threads = CGD_MALLOC("cgd.threads.it.1", VG_N_THREADS * sizeof threads[0]);

	for(i=0;i<VG_N_THREADS;i++)
		threads[i] = 0;

	CGD_(current_tid) = VG_INVALID_THREADID;
}

/* switches through all threads and calls func */
void CGD_(forall_threads)(void (*func)(thread_info*))
{
  Int t, orig_tid = CGD_(current_tid);

  for(t=1;t<VG_N_THREADS;t++) {
    if (!threads[t]) continue;
    CGD_(switch_thread)(t);
    (*func)(threads[t]);
  }
  CGD_(switch_thread)(orig_tid);
}


static
thread_info* new_thread(void)
{
    thread_info* t;

    t = (thread_info*) CGD_MALLOC("cgd.threads.nt.1",
                                  sizeof(thread_info));

    /* init state */
    CGD_(init_exec_stack)( &(t->states) );
    CGD_(init_call_stack)( &(t->calls) );

    return t;
}

static
void delete_thread(thread_info* t) {
	CGD_ASSERT(t != 0);

	/* destroy state */
	CGD_(destroy_call_stack)(&(t->calls));
	CGD_(destroy_exec_stack)(&(t->states));

	CGD_DATA_FREE(t, sizeof(thread_info));
}

void CGD_(destroy_threads)() {
	UInt i;

	for (i = 0; i < VG_N_THREADS; i++) {
		if (threads[i]) {
			// Update the thread info for the current thread.
			if (CGD_(current_tid) == i) {
				CGD_(copy_current_exec_stack)(&(threads[i]->states));
				CGD_(copy_current_call_stack)(&(threads[i]->calls));
			}

			delete_thread(threads[i]);
			threads[i] = 0;
		}
	}

	CGD_FREE(threads);
	threads = 0;

	CGD_(current_tid) = VG_INVALID_THREADID;
}

void CGD_(switch_thread)(ThreadId tid)
{
  if (tid == CGD_(current_tid)) return;

  CGD_DEBUG(0, ">> thread %u (was %u)\n", tid, CGD_(current_tid));

  if (CGD_(current_tid) != VG_INVALID_THREADID) {    
    /* save thread state */
    thread_info* t = threads[CGD_(current_tid)];

    CGD_ASSERT(t != 0);

    /* current context (including signal handler contexts) */
    exec_state_save();
    CGD_(copy_current_exec_stack)( &(t->states) );
    CGD_(copy_current_call_stack)( &(t->calls) );
  }

  CGD_(current_tid) = tid;
  CGD_ASSERT(tid < VG_N_THREADS);

  if (tid != VG_INVALID_THREADID) {
    thread_info* t;

    /* load thread state */
    if (threads[tid] == 0)
      threads[tid] = new_thread();

    t = threads[tid];

    /* current context (including signal handler contexts) */
    CGD_(set_current_exec_stack)( &(t->states) );
    exec_state_restore();
    CGD_(set_current_call_stack)( &(t->calls) );
  }
}


void CGD_(run_thread)(ThreadId tid)
{
    /* now check for thread switch */
    CGD_(switch_thread)(tid);
}

void CGD_(pre_signal)(ThreadId tid, Int sigNum, Bool alt_stack)
{
    exec_state* es;
	exec_state* old_es;

    CGD_DEBUG(0, ">> pre_signal(TID %u, sig %d, alt_st %s)\n",
	     tid, sigNum, alt_stack ? "yes":"no");

    /* switch to the thread the handler runs in */
    CGD_(switch_thread)(tid);

    /* save current execution state */
    old_es = exec_state_save();

    /* setup new cxtinfo struct for this signal handler */
    es = push_exec_state(sigNum);
    es->call_stack_bottom = CGD_(current_call_stack).sp;

    /* setup current state for a spontaneous call */
    CGD_(init_exec_state)( &CGD_(current_state) );
    CGD_(current_state).sig = sigNum;

	// Restore CFG and working for signal mapping.
	CGD_(current_state).cfg = old_es->cfg;
	CGD_(current_state).working = old_es->working;
}

/* Run post-signal if the stackpointer for call stack is at
 * the bottom in current exec state (e.g. a signal handler)
 *
 * Called from CGD_(pop_call_stack)
 */
void CGD_(run_post_signal_on_call_stack_bottom)()
{
    exec_state* es = top_exec_state();
    CGD_ASSERT(es != 0);
    CGD_ASSERT(CGD_(current_state).sig >0);

    if (CGD_(current_call_stack).sp == es->call_stack_bottom)
	CGD_(post_signal)( CGD_(current_tid), CGD_(current_state).sig );
}

void CGD_(post_signal)(ThreadId tid, Int sigNum)
{
    exec_state* es;

    CGD_DEBUG(0, ">> post_signal(TID %u, sig %d)\n",
	     tid, sigNum);

    /* thread switching potentially needed, eg. with instrumentation off */
    CGD_(switch_thread)(tid);
    CGD_ASSERT(sigNum == CGD_(current_state).sig);

    /* Unwind call stack of this signal handler.
     * This should only be needed at finalisation time
     */
    es = top_exec_state();
    CGD_ASSERT(es != 0);
    while(CGD_(current_call_stack).sp > es->call_stack_bottom)
      CGD_(pop_call_stack)(False);

    // Connect the end of the signal handler to the exit node.
    CGD_(cfgnode_set_exit)(CGD_(current_state).cfg, CGD_(current_state).working);

    /* restore previous context */
    es->sig = -1;
    current_states.sp--;
    es = top_exec_state();
    CGD_(current_state).sig = es->sig;
    exec_state_restore();

    /* There is no way to reliable get the thread ID we are switching to
     * after this handler returns. So we sync with actual TID at start of
     * CGD_(setup_bb)(), which should be the next for cfggrind.
     */
}



/*------------------------------------------------------------*/
/*--- Execution states in a thread & signal handlers       ---*/
/*------------------------------------------------------------*/

/* Each thread can be interrupted by a signal handler, and they
 * themselves again. But as there's no scheduling among handlers
 * of the same thread, we don't need additional stacks.
 * So storing execution contexts and
 * adding separators in the callstack(needed to not intermix normal/handler
 * functions in contexts) should be enough.
 */

/* not initialized: call_stack_bottom, sig */
void CGD_(init_exec_state)(exec_state* es)
{
  es->jmps_passed = 0;
  es->bb = 0;
  es->cfg = 0;
  es->working = 0;
}


static exec_state* new_exec_state(Int sigNum)
{
    exec_state* es;
    es = (exec_state*) CGD_MALLOC("cgd.threads.nes.1",
                                  sizeof(exec_state));

    /* allocate real cost space: needed as incremented by
     * simulation functions */
    CGD_(init_exec_state)(es);
    es->sig        = sigNum;
    es->call_stack_bottom  = 0;

    return es;
}

void CGD_(init_exec_stack)(exec_stack* es)
{
  Int i;

  CGD_ASSERT(es != 0);

  /* The first element is for the main thread */
  es->entry[0] = new_exec_state(0);
  for(i=1;i<MAX_SIGHANDLERS;i++)
    es->entry[i] = 0;
  es->sp = 0;
}

void CGD_(destroy_exec_stack)(exec_stack* es) {
	Int i;

	CGD_ASSERT(es != 0);

	for (i = 0; i < MAX_SIGHANDLERS; i++) {
		if (es->entry[i]) {
			CGD_FREE(es->entry[i]);
			es->entry[i] = 0;
		}
	}
}

void CGD_(copy_current_exec_stack)(exec_stack* dst)
{
  Int i;

  dst->sp = current_states.sp;
  for(i=0;i<MAX_SIGHANDLERS;i++)
    dst->entry[i] = current_states.entry[i];
}

void CGD_(set_current_exec_stack)(exec_stack* dst)
{
  Int i;

  current_states.sp = dst->sp;
  for(i=0;i<MAX_SIGHANDLERS;i++)
    current_states.entry[i] = dst->entry[i];
}


/* Get top context info struct of current thread */
static
exec_state* top_exec_state(void)
{
  Int sp = current_states.sp;
  exec_state* es;

  CGD_ASSERT((sp >= 0) && (sp < MAX_SIGHANDLERS));
  es = current_states.entry[sp];
  CGD_ASSERT(es != 0);
  return es;
}

/* Allocates a free context info structure for a new entered
 * signal handler, putting it on the context stack.
 * Returns a pointer to the structure.
 */
static exec_state* push_exec_state(int sigNum)
{   
  Int sp;
  exec_state* es;

  current_states.sp++;
  sp = current_states.sp;

  CGD_ASSERT((sigNum > 0) && (sigNum <= _VKI_NSIG));
  CGD_ASSERT((sp > 0) && (sp < MAX_SIGHANDLERS));
  es = current_states.entry[sp];
  if (!es) {
    es = new_exec_state(sigNum);
    current_states.entry[sp] = es;
  }
  else
    es->sig = sigNum;

  return es;
}

/* Save current context to top cxtinfo struct */
static
exec_state* exec_state_save(void)
{
  exec_state* es = top_exec_state();

  es->jmps_passed  = CGD_(current_state).jmps_passed;
  es->bb           = CGD_(current_state).bb;
  es->cfg          = CGD_(current_state).cfg;
  es->working     = CGD_(current_state).working;

  CGD_DEBUGIF(1) {
    CGD_DEBUG(1, "  cxtinfo_save(sig %d): jmps_passed %d\n",
	     es->sig, es->jmps_passed);
    CGD_(print_bb)(-9, es->bb);
  }

  /* signal number does not need to be saved */
  CGD_ASSERT(CGD_(current_state).sig == es->sig);

  return es;
}

static
exec_state* exec_state_restore(void)
{
  exec_state* es = top_exec_state();
  
  CGD_(current_state).jmps_passed  = es->jmps_passed;
  CGD_(current_state).bb           = es->bb;
  CGD_(current_state).sig          = es->sig;
  CGD_(current_state).cfg          = es->cfg;
  CGD_(current_state).working     = es->working;

  CGD_DEBUGIF(1) {
	CGD_DEBUG(1, "  exec_state_restore(sig %d): jmps_passed %d\n",
		  es->sig, es->jmps_passed);
  }

  return es;
}
