/*--------------------------------------------------------------------*/
/*--- CFGgrind                                                     ---*/
/*---                                                        clo.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of CFGgrind, a dynamic control flow graph (CFG)
   reconstruction tool.

   Copyright (C) 2022, Andrei Rimsa (andrei@cefetmg.br)

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

#include "config.h" // for VG_PREFIX

#include "global.h"



/*------------------------------------------------------------*/
/*--- Function specific configuration options              ---*/
/*------------------------------------------------------------*/

#define CONFIG_DEFAULT -1
#define CONFIG_FALSE    0
#define CONFIG_TRUE     1

/*--------------------------------------------------------------------*/
/*--- Command line processing                                      ---*/
/*--------------------------------------------------------------------*/

Bool CGD_(process_cmd_line_option)(const HChar* arg)
{
   const HChar* tmp_str;

   if (False) {}
#if CGD_ENABLE_DEBUG
   else if VG_INT_CLO(arg, "--ct-verbose", CGD_(clo).verbose) {}
   else if VG_INT_CLO(arg, "--ct-vstart",  CGD_(clo).verbose_start) {}
#endif

   else if VG_STR_CLO(arg, "--cfg-outfile", CGD_(clo).cfg_outfile) {}
   else if VG_STR_CLO(arg, "--cfg-infile", CGD_(clo).cfg_infile) {}
   else if VG_BOOL_CLO(arg, "--ignore-failed-cfg", CGD_(clo).ignore_failed) {}
#if ENABLE_PROFILING
   else if VG_BOOL_CLO(arg, "--ignore-profiling", CGD_(clo).ignore_profiling) {}
#endif
   else if VG_BOOL_CLO(arg, "--emulate-calls", CGD_(clo).emulate_calls) {}
   else if VG_STR_CLO(arg, "--cfg-dump", tmp_str) {
	   if (VG_(strcasecmp)(tmp_str, "all") == 0) {
		   CGD_ASSERT(CGD_(clo).dump_cfgs.all == False);
		   CGD_ASSERT(CGD_(clo).dump_cfgs.addrs == 0);
		   CGD_ASSERT(CGD_(clo).dump_cfgs.fnames == 0);

		   CGD_(clo).dump_cfgs.all = True;
	   } else if (VG_(strcasecmp)(tmp_str, "none") == 0) {
		   CGD_ASSERT(CGD_(clo).dump_cfgs.all == False);
		   CGD_ASSERT(CGD_(clo).dump_cfgs.addrs == 0);
		   CGD_ASSERT(CGD_(clo).dump_cfgs.fnames == 0);
	   } else if (VG_(strncasecmp)(tmp_str, "0x", 2) == 0) {
		   Addr addr;

		   CGD_ASSERT(CGD_(clo).dump_cfgs.all == False);

		   addr = VG_(strtoull16)(tmp_str, 0);
		   CGD_ASSERT(addr != 0);

		   if (CGD_(clo).dump_cfgs.addrs == 0)
			   CGD_(clo).dump_cfgs.addrs = CGD_(new_smart_list)(1);

		   CGD_(smart_list_add)(CGD_(clo).dump_cfgs.addrs, (void*) addr);
	   } else {
		   CGD_ASSERT(CGD_(clo).dump_cfgs.all == False);

		   if (CGD_(clo).dump_cfgs.fnames == 0)
			   CGD_(clo).dump_cfgs.fnames = CGD_(new_smart_list)(1);

		   CGD_(smart_list_add)(CGD_(clo).dump_cfgs.fnames,
				   (void*) CGD_STRDUP("cgd.clo.pclo.1", tmp_str));
	   }
   }
   else if VG_STR_CLO(arg, "--cfg-dump-dir", CGD_(clo).dump_cfgs.dir) {}
   else if VG_STR_CLO(arg, "--instrs-map", CGD_(clo).instrs_map) {}
   else if VG_STR_CLO(arg, "--mem-mappings", CGD_(clo).mem_mappings) {}

   else
	   return False;

   return True;
}

void CGD_(print_usage)(void)
{
   VG_(printf)(
"\n   control flow graph options:\n"
"    --cfg-outfile=<f>            CFG output file name\n"
"		  use %%p to bind the pid to a cfg file (e.g. cfggrind.%%p.cfg)\n"
"    --cfg-infile=<f>             CFG input file name\n"
"    --ignore-failed-cfg=no|yes   Ignore failed cfg input file read [no]\n"
#if ENABLE_PROFILING
"    --ignore-profiling=no|yes    Ignore profiling information from input file [no]\n"
#endif
"    --emulate-calls=no|yes       Emulate call for jumps in function entries [yes]\n"
"    --cfg-dump=<name>            Dump DOT cfg file as cfg-<name>.dot [none]\n"
"		  where <name> is a function name, an address (e.g. 0xNNNNNNNN), all or none\n"
"         (can be used multiple times)\n"
"    --cfg-dump-dir=<directory>   Directory where to dump the DOT cfgs [.]\n"
"    --instrs-map=<f>             Instructions map (address:size:assembly per entry) file\n"
"    --mem-mappings=<f>           Output file with memory mappings (bin, libs, ...)\n"
    );
}

void CGD_(print_debug_usage)(void)
{
    VG_(printf)(

#if CGD_ENABLE_DEBUG
"    --ct-verbose=<level>       Verbosity of standard debug output [0]\n"
"    --ct-vstart=<BB number>    Only be verbose after basic block [0]\n"
"    --ct-verbose<level>=<func> Verbosity while in <func>\n"
#else
"    (none)\n"
#endif

    );
}

void CGD_(set_clo_defaults)(void)
{
  /* Default values for command line arguments */

  /* cfg options */
  CGD_(clo).cfg_outfile      = 0;
  CGD_(clo).cfg_infile       = 0;
  CGD_(clo).ignore_failed    = False;
#if ENABLE_PROFILING
  CGD_(clo).ignore_profiling = False;
#endif
  CGD_(clo).emulate_calls    = True;
  CGD_(clo).dump_cfgs.all    = False;
  CGD_(clo).dump_cfgs.addrs  = 0;
  CGD_(clo).dump_cfgs.fnames = 0;
  CGD_(clo).dump_cfgs.dir    = ".";
  CGD_(clo).instrs_map       = 0;
  CGD_(clo).mem_mappings     = 0;

#if CGD_ENABLE_DEBUG
  CGD_(clo).verbose = 0;
  CGD_(clo).verbose_start = 0;
#endif
}
