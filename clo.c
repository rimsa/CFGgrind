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

Bool LPG_(process_cmd_line_option)(const HChar* arg)
{
   const HChar* tmp_str;

   if (False) {}
#if LPG_ENABLE_DEBUG
   else if VG_INT_CLO(arg, "--ct-verbose", LPG_(clo).verbose) {}
   else if VG_INT_CLO(arg, "--ct-vstart",  LPG_(clo).verbose_start) {}
#endif

   else if VG_STR_CLO(arg, "--cfg-outfile", LPG_(clo).cfg_outfile) {}
   else if VG_STR_CLO(arg, "--cfg-infile", LPG_(clo).cfg_infile) {}
   else if VG_BOOL_CLO(arg, "--ignore-failed-cfg", LPG_(clo).ignore_failed) {}
   else if VG_STR_CLO(arg, "--cfg-dump", tmp_str) {
	   if (VG_(strcmp)(tmp_str, "all") == 0) {
		   LPG_ASSERT(LPG_(clo).dump_cfgs.all == False);
		   LPG_ASSERT(LPG_(clo).dump_cfgs.addrs == 0);
		   LPG_ASSERT(LPG_(clo).dump_cfgs.fnames == 0);

		   LPG_(clo).dump_cfgs.all = True;
	   } else if (VG_(strcmp)(tmp_str, "none") == 0) {
		   LPG_ASSERT(LPG_(clo).dump_cfgs.all == False);
		   LPG_ASSERT(LPG_(clo).dump_cfgs.addrs == 0);
		   LPG_ASSERT(LPG_(clo).dump_cfgs.fnames == 0);
	   } else if (VG_(strncmp)(tmp_str, "0x", 2) == 0) {
		   Addr addr;

		   LPG_ASSERT(LPG_(clo).dump_cfgs.all == False);

		   addr = VG_(strtoull16)(tmp_str, 0);
		   LPG_ASSERT(addr != 0);

		   if (LPG_(clo).dump_cfgs.addrs == 0)
			   LPG_(clo).dump_cfgs.addrs = LPG_(new_smart_list)(1);

		   LPG_(smart_list_add)(LPG_(clo).dump_cfgs.addrs, (void*) addr);
	   } else {
		   LPG_ASSERT(LPG_(clo).dump_cfgs.all == False);

		   if (LPG_(clo).dump_cfgs.fnames == 0)
			   LPG_(clo).dump_cfgs.fnames = LPG_(new_smart_list)(1);

		   LPG_(smart_list_add)(LPG_(clo).dump_cfgs.fnames,
				   (void*) LPG_STRDUP("lg.clo.pclo.1", tmp_str));
	   }
   }
   else if VG_STR_CLO(arg, "--cfg-dump-dir", LPG_(clo).dump_cfgs.dir) {}
   else if VG_STR_CLO(arg, "--instrs-map", LPG_(clo).instrs_map) {}

   else
	   return False;

   return True;
}

void LPG_(print_usage)(void)
{
   VG_(printf)(
"\n   control flow graph options:\n"
"    --cfg-outfile=<f>            CFG output file name\n"
"    --cfg-infile=<f>             CFG input file name\n"
"    --ignore-failed-cfg=no|yes   Ignore failed cfg input file read [no]\n"
"    --cfg-dump=<name>            Dump DOT cfg file as cfg-<name>.dot [none]\n"
"		  where <name> is a function name, an address (e.g. 0xNNNNNNNN), all or none\n"
"         (can be used multiple times)\n"
"    --cfg-dump-dir=<directory>   Directory where to dump the DOT cfgs [.]\n"
"    --instrs-map=<f>             Instructions map (address:assembly per entry) file\n"
    );

//   VG_(printf)("\n"
//	       "  For full callgrind documentation, see\n"
//	       "  "VG_PREFIX"/share/doc/callgrind/html/callgrind.html\n\n");
}

void LPG_(print_debug_usage)(void)
{
    VG_(printf)(

#if LPG_ENABLE_DEBUG
"    --ct-verbose=<level>       Verbosity of standard debug output [0]\n"
"    --ct-vstart=<BB number>    Only be verbose after basic block [0]\n"
"    --ct-verbose<level>=<func> Verbosity while in <func>\n"
#else
"    (none)\n"
#endif

    );
}


void LPG_(set_clo_defaults)(void)
{
  /* Default values for command line arguments */

  /* cfg options */
  LPG_(clo).cfg_outfile      = 0;
  LPG_(clo).cfg_infile       = 0;
  LPG_(clo).ignore_failed    = False;
  LPG_(clo).dump_cfgs.all    = False;
  LPG_(clo).dump_cfgs.addrs  = 0;
  LPG_(clo).dump_cfgs.fnames = 0;
  LPG_(clo).dump_cfgs.dir    = ".";
  LPG_(clo).instrs_map       = 0;

#if LPG_ENABLE_DEBUG
  LPG_(clo).verbose = 0;
  LPG_(clo).verbose_start = 0;
#endif
}
