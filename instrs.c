/*--------------------------------------------------------------------*/
/*--- CFGgrind                                                     ---*/
/*---                                                     instrs.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of CFGgrind, a dynamic control flow graph (CFG)
   reconstruction tool.

   Copyright (C) 2023, Andrei Rimsa (andrei@cefetmg.br)

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

typedef struct _instrs_hash instrs_hash;
struct _instrs_hash {
	UInt size, entries;
	UniqueInstr** table;
};

instrs_hash pool;

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

static __inline__
UInt instrs_hash_idx(Addr addr, UInt size) {
	return addr % size;
}

static
void resize_instrs_pool(void) {
    Int i, new_size, conflicts1 = 0;
    UniqueInstr **new_table, *curr, *next;
    UInt new_idx;

	// increase table by 50%.
    new_size  = (Int) (1.5f * pool.size);
    new_table = (UniqueInstr**) CGD_MALLOC("cgd.instrs.rit.1",
                                  (new_size * sizeof(UniqueInstr*)));
    VG_(memset)(new_table, 0, (new_size * sizeof(UniqueInstr*)));

    for (i = 0; i < pool.size; i++) {
		if (pool.table[i] == 0)
			continue;

		curr = pool.table[i];
		while (curr != 0) {
			next = curr->chain;

			new_idx = instrs_hash_idx(curr->addr, new_size);

			curr->chain = new_table[new_idx];
			new_table[new_idx] = curr;
			if (curr->chain)
				conflicts1++;

			curr = next;
		}
    }

    CGD_FREE(pool.table);

    CGD_DEBUG(0, "Resize instructions pool: %u => %d (entries %u, conflicts %d)\n",
	     pool.size, new_size,
	     pool.entries, conflicts1);

    pool.size  = new_size;
    pool.table = new_table;
    CGD_(stat).instrs_pool_resizes++;
}

static
UniqueInstr* lookup_instr(Addr addr) {
	UniqueInstr* instr;
	UInt idx;

	CGD_ASSERT(addr != 0);

	idx = instrs_hash_idx(addr, pool.size);
	instr = pool.table[idx];

	while (instr) {
		if (instr->addr == addr)
			break;

		instr = instr->chain;
	}

	return instr;
}

void CGD_(init_instrs_pool)(void) {
	Int size;

	pool.size = DEFAULT_POOL_SIZE;
	pool.entries = 0;

	size = pool.size * sizeof(UniqueInstr*);
	pool.table = (UniqueInstr**) CGD_MALLOC("cgd.instrs.iip.1", size);
	VG_(memset)(pool.table, 0, size);

	// read instruction names.
	read_instr_names();
}

void CGD_(destroy_instrs_pool)(void) {
	Int i;

	for (i = 0; i < pool.size; i++) {
		UniqueInstr* instr = pool.table[i];
		while (instr) {
			UniqueInstr* next = instr->chain;
			delete_instr(instr);
			instr = next;

			pool.entries--;
		}
	}

	CGD_ASSERT(pool.entries == 0);

	CGD_FREE(pool.table);
	pool.table = 0;
}

UniqueInstr* CGD_(get_instr)(Addr addr, Int size) {
	UniqueInstr* instr = lookup_instr(addr);
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
		UInt idx;

		/* check fill degree of instructions pool and resize if needed (>80%) */
		pool.entries++;
		if (10 * pool.entries / pool.size > 8)
			resize_instrs_pool();

		// Create the instruction.
		instr = (UniqueInstr*) CGD_MALLOC("cgd.instrs.gi.1", sizeof(UniqueInstr));
		VG_(memset)(instr, 0, sizeof(UniqueInstr));
		instr->addr = addr;
		instr->size = size;

		/* insert into instructions pool */
		idx = instrs_hash_idx(addr, pool.size);
		instr->chain = pool.table[idx];
		pool.table[idx] = instr;
	}

	return instr;
}

UniqueInstr* CGD_(find_instr)(Addr addr) {
	return lookup_instr(addr);
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
