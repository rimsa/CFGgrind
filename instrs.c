/*--------------------------------------------------------------------*/
/*--- CFGgrind                                                     ---*/
/*---                                                     instrs.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of CFGgrind, a dynamic control flow graph (CFG)
   reconstruction tool.

   Copyright (C) 2019, Andrei Rimsa (andrei@cefetmg.br)

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

#define DEFAULT_POOL_SIZE 262144 // 256k instructions

SmartHash* instrs_pool = 0;

static
void delete_instr(UniqueInstr* instr) {
	CGD_ASSERT(instr != 0);

	if (instr->name)
		CGD_FREE(instr->name);

	if (instr->desc) {
		if (instr->desc->name != 0)
			CGD_FREE(instr->desc->name);

		CGD_DATA_FREE(instr->desc, sizeof(InstrDesc));
	}

	CGD_DATA_FREE(instr, sizeof(UniqueInstr));
}

static
HChar* next_line(Int fd) {
    Int s, idx;
    HChar c;
    static HChar buffer[1024];

    idx = 0;
    VG_(memset)(&buffer, 0, sizeof(buffer));

    while (True) {
		CGD_ASSERT(idx >= 0 && idx < ((sizeof(buffer) / sizeof(HChar))-1));
		s = VG_(read)(fd, &c, 1);
		if (s == 0 || c == '\n')
			break;

		// Ignore carriage returns.
		if (c == '\r')
			continue;

		buffer[idx++] = c;
    }

    return idx > 0 ? buffer : 0;
}

static
void read_instr_names(void) {
	Int fd;
	Int size;
	Addr addr;
	HChar* line;
	HChar* tmp;
	HChar* name;
	UniqueInstr* instr;

	if (CGD_(clo).instrs_map) {
		fd = VG_(fd_open)(CGD_(clo).instrs_map, VKI_O_RDONLY, 0);
		if (fd < 0)
			tl_assert(0);

		while ((line = next_line(fd))) {
			tmp = VG_(strchr)(line, ':');
			if (tmp == 0)
				continue;
			*tmp = 0;
			++tmp;

			name = VG_(strchr)(tmp, ':');
			if (name == 0)
				continue;
			*name = 0;
			++name;

			addr = VG_(strtoull16)(line, 0);
			size = VG_(strtoll10)(tmp, 0);
			if (addr != 0 && size > 0 && *name != 0) {
				instr = CGD_(get_instr)(addr, size);
				instr->name = CGD_STRDUP("cgd.instrs.rin.1", name);
			}
		}

		VG_(close)(fd);
	}
}

void CGD_(init_instrs_pool)() {
	CGD_ASSERT(instrs_pool == 0);

	instrs_pool = CGD_(new_smart_hash)(DEFAULT_POOL_SIZE);

	// set the growth rate to half the size.
	CGD_(smart_hash_set_growth_rate)(instrs_pool, 1.5f);

	// read instruction names.
	read_instr_names();
}

void CGD_(destroy_instrs_pool)() {
	CGD_ASSERT(instrs_pool != 0);

	CGD_(smart_hash_clear)(instrs_pool, (void (*)(void*)) delete_instr);
	CGD_(delete_smart_hash)(instrs_pool);
	instrs_pool = 0;
}

UniqueInstr* CGD_(get_instr)(Addr addr, Int size) {
	UniqueInstr* instr = CGD_(find_instr)(addr);
	if (instr) {
		CGD_ASSERT(instr->addr == addr);
		if (size != 0) {
			if (instr->size == 0) {
				instr->size = size;
			} else {
				CGD_ASSERT(instr->size == size);
			}
		}
	} else {
		instr = (UniqueInstr*) CGD_MALLOC("cgd.instrs.gi.1", sizeof(UniqueInstr));
		VG_(memset)(instr, 0, sizeof(UniqueInstr));
		instr->addr = addr;
		instr->size = size;

		CGD_(smart_hash_put)(instrs_pool, instr, (HWord (*)(void*)) CGD_(instr_addr));
	}

	return instr;
}

UniqueInstr* CGD_(find_instr)(Addr addr) {
	return (UniqueInstr*) CGD_(smart_hash_get)(instrs_pool, addr, (HWord (*)(void*)) CGD_(instr_addr));
}

Addr CGD_(instr_addr)(UniqueInstr* instr) {
	CGD_ASSERT(instr != 0);
	return instr->addr;
}

Int CGD_(instr_size)(UniqueInstr* instr) {
	CGD_ASSERT(instr != 0);
	return instr->size;
}

const HChar* CGD_(instr_name)(UniqueInstr* instr) {
	CGD_ASSERT(instr != 0);
	return instr->name;
}

InstrDesc* CGD_(instr_description)(UniqueInstr* instr) {
	CGD_ASSERT(instr != 0);

	if (!instr->desc) {
		Bool found;
		DiEpoch ep;
		UInt tmpline;
		const HChar *tmpfile, *tmpdir;

		ep = VG_(current_DiEpoch)();
		found = VG_(get_filename_linenum)(ep, instr->addr,
					&(tmpfile), &(tmpdir), &(tmpline));

		instr->desc = (InstrDesc*) CGD_MALLOC("cgd.instrs.id.1", sizeof(InstrDesc));
		if (found) {
		    /* Build up an absolute pathname, if there is a directory available */
			instr->desc->name = (HChar*) CGD_MALLOC("cgd.adesc.na.1",
		    		(VG_(strlen)(tmpdir) + 1 + VG_(strlen)(tmpfile) + 1));
		    VG_(strcpy)(instr->desc->name, tmpdir);
		    if (instr->desc->name[0] != '\0')
		       VG_(strcat)(instr->desc->name, "/");
		    VG_(strcat)(instr->desc->name, tmpfile);

			instr->desc->lineno = tmpline;
		} else {
			instr->desc->name = 0;
			instr->desc->lineno = -1;
		}
	}

	return instr->desc;
}

Bool CGD_(instrs_cmp)(UniqueInstr* i1, UniqueInstr* i2) {
	return i1 && i2 && i1->addr == i2->addr && i1->size == i2->size;
}

void CGD_(print_instr)(UniqueInstr* instr, Bool complete) {
	CGD_ASSERT(instr != 0);

	VG_(printf)("0x%lx [%d]", instr->addr, instr->size);
	if (complete) {
		VG_(printf)(" (");
		CGD_(print_instr_description)(CGD_(instr_description)(instr));
		VG_(printf)(")");
	}
}

void CGD_(fprint_instr)(VgFile* fp, UniqueInstr* instr, Bool complete) {
	CGD_ASSERT(fp != 0);
	CGD_ASSERT(instr != 0);

	VG_(fprintf)(fp, "0x%lx [%d]", instr->addr, instr->size);
	if (complete) {
		VG_(fprintf)(fp, " (");
		CGD_(fprint_instr_description)(fp, CGD_(instr_description)(instr));
		VG_(fprintf)(fp, ")");
	}
}

void CGD_(print_instr_description)(InstrDesc* idesc) {
	CGD_ASSERT(idesc != 0);

	if (idesc->name)
		VG_(printf)("%s:%d", idesc->name, idesc->lineno);
	else
		VG_(printf)("unknown");
}

void CGD_(fprint_instr_description)(VgFile* fp, InstrDesc* idesc) {
	CGD_ASSERT(fp != 0);
	CGD_ASSERT(idesc != 0);

	if (idesc->name)
		VG_(fprintf)(fp, "%s:%d", idesc->name, idesc->lineno);
	else
		VG_(fprintf)(fp, "unknown");
}
