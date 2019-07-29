/*
   This file is part of Callgrind, a Valgrind tool for call graph
   profiling programs.

   Copyright (C) 2002-2017, Josef Weidendorfer (Josef.Weidendorfer@gmx.de)

   This tool is derived from and contains lot of code from Cachegrind
   Copyright (C) 2002-2017 Nicholas Nethercote (njn@valgrind.org)

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

/* If debugging mode of, dummy functions are provided (see below)
 */
#if LPG_ENABLE_DEBUG

/*------------------------------------------------------------*/
/*--- Debug output helpers                                 ---*/
/*------------------------------------------------------------*/

static void print_indent(int s)
{
    /* max of 40 spaces */
    const HChar sp[] = "                                        ";
    if (s>40) s=40;
    VG_(printf)("%s", sp+40-s);
}

void LPG_(print_bb)(int s, BB* bb)
{
    if (s<0) {
	s = -s;
	print_indent(s);
    }

    VG_(printf)("BB %#lx (Obj '%s')", bb_addr(bb), bb->obj->name);
}

void LPG_(print_execstate)(int s, exec_state* es)
{
  if (s<0) {
    s = -s;
    print_indent(s);
  }
  
  if (!es) {
    VG_(printf)("ExecState 0x0\n");
    return;
  }

  VG_(printf)("ExecState [Sig %d]: jmps_passed %d\n",
	      es->sig, es->jmps_passed);
}

/* dump out the current call stack */
void LPG_(print_stackentry)(int s, int sp)
{
    call_entry* ce;

    if (s<0) {
	s = -s;
	print_indent(s);
    }

    ce = LPG_(get_call_entry)(sp);
    VG_(printf)("[%-2d] SP %#lx, RA %#lx\n", sp, ce->sp, ce->ret_addr);
}

/* debug output */
#if 0
static void print_call_stack()
{
    int c;

    VG_(printf)("Call Stack:\n");
    for(c=0;c<LPG_(current_call_stack).sp;c++)
      LPG_(print_stackentry)(-2, c);
}
#endif

/* dump out an address with source info if available */
void LPG_(print_addr)(Addr addr)
{
    const HChar *fn_buf, *fl_buf, *dir_buf;
    const HChar* obj_name;
    DebugInfo* di;
    UInt ln, i=0, opos=0;
	
    if (addr == 0) {
	VG_(printf)("%08lx", addr);
	return;
    }

    LPG_(get_debug_info)(addr, &dir_buf, &fl_buf, &fn_buf, &ln, &di);

    if (VG_(strcmp)(fn_buf,"???")==0)
	VG_(printf)("%#lx", addr);
    else
	VG_(printf)("%#lx %s", addr, fn_buf);

    if (di) {
      obj_name = VG_(DebugInfo_get_filename)(di);
      if (obj_name) {
	while(obj_name[i]) {
	  if (obj_name[i]=='/') opos = i+1;
	  i++;
	}
	if (obj_name[0])
	  VG_(printf)(" %s", obj_name+opos);
      }
    }

    if (ln>0) {
       if (dir_buf[0])
          VG_(printf)(" (%s/%s:%u)", dir_buf, fl_buf, ln);
       else
          VG_(printf)(" (%s:%u)", fl_buf, ln);
    }
}

void LPG_(print_addr_ln)(Addr addr)
{
  LPG_(print_addr)(addr);
  VG_(printf)("\n");
}

static ULong bb_written = 0;

void LPG_(print_bbno)(void)
{
  if (bb_written != LPG_(stat).bb_executions) {
    bb_written = LPG_(stat).bb_executions;
    VG_(printf)("BB# %llu\n",LPG_(stat).bb_executions);
  }
}

#if LPG_DEBUG_MEM
void* LPG_(malloc)(const HChar* cc, UWord s, const HChar* f) {
	void* p;

	LPG_UNUSED(cc);

	LPG_DEBUG(3, "Malloc(%lu) in %s: ", s, f);
	p = VG_(malloc)(cc, s);
	LPG_DEBUG(3, "%p\n", p);
	return p;
}

void* LPG_(realloc)(const HChar* cc, void* p, UWord s, const HChar* f) {
	LPG_UNUSED(cc);

	if (p != 0)
		LPG_DEBUG(3, "Free in %s: %p\n", f, p);

	LPG_DEBUG(3, "Malloc(%lu) in %s: ", s, f);
	p = VG_(realloc)(cc, p, s);
	LPG_DEBUG(3, "%p\n", p);
	return p;
}

void LPG_(free)(void* p, const HChar* f) {
	LPG_DEBUG(3, "Free in %s: %p\n", f, p);
	VG_(free)(p);
}

HChar* LPG_(strdup)(const HChar* cc, const HChar* s, const HChar* f) {
	HChar* p;

	LPG_UNUSED(cc);

	LPG_DEBUG(3, "Strdup(%s) in %s: ", s, f);
	p = VG_(strdup)(cc, s);
	LPG_DEBUG(3, "%p\n", p);
	return p;
}
#endif

#else /* LPG_ENABLE_DEBUG */

void LPG_(print_bbno)(void) {}
void LPG_(print_bb)(int s, BB* bb) {}
void LPG_(print_stackentry)(int s, int sp) {}
void LPG_(print_addr)(Addr addr) {}
void LPG_(print_addr_ln)(Addr addr) {}

#endif
