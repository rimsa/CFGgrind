/*--------------------------------------------------------------------*/
/*--- CFGgrind                                                     ---*/
/*---                                                      fdesc.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of CFGgrind, a dynamic control flow graph (CFG)
   reconstruction tool.

   Copyright (C) 2022, Andrei Rimsa (andrei@cefetmg.br)

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

static
const HChar* unknown_name = "???";

static
const HChar* main_fname = "main";

struct _FunctionDesc {
	HChar* obj_name;
	HChar* fn_name;
	UInt fn_line;
};

FunctionDesc* CGD_(new_fdesc)(Addr addr, Bool entry) {
	const HChar* tmp;
	FunctionDesc* fdesc;
	Bool found;

	DiEpoch ep = VG_(current_DiEpoch)();
	found = entry ?
			VG_(get_fnname_if_entry)(ep, addr, &tmp) :
			VG_(get_fnname)(ep, addr, &tmp);

	if (found) {
		fdesc = (FunctionDesc*) CGD_MALLOC("cgd.fdesc.nf.1", sizeof(FunctionDesc));

		fdesc->fn_name = CGD_STRDUP("cgd.fdesc.nf.2", tmp);
		if (!VG_(get_linenum)(ep, addr, &(fdesc->fn_line)))
			fdesc->fn_line = 0;

		fdesc->obj_name = VG_(get_objname)(ep, addr, &tmp) ?
								CGD_STRDUP("cgd.fdesc.nf.3", tmp) : 0;
	} else {
		fdesc = 0;
	}

	return fdesc;
}

void CGD_(delete_fdesc)(FunctionDesc* fdesc) {
	CGD_ASSERT(fdesc);
	CGD_ASSERT(fdesc->fn_name);

	CGD_DATA_FREE(fdesc->fn_name, VG_(strlen)(fdesc->fn_name));
	if (fdesc->obj_name)
		CGD_DATA_FREE(fdesc->obj_name, VG_(strlen)(fdesc->obj_name));

	CGD_DATA_FREE(fdesc, sizeof(FunctionDesc));
}

HChar* CGD_(fdesc_object_name)(FunctionDesc* fdesc) {
	CGD_ASSERT(fdesc != 0);
	return fdesc->obj_name;
}

HChar* CGD_(fdesc_function_name)(FunctionDesc* fdesc) {
	CGD_ASSERT(fdesc != 0);
	return fdesc->fn_name;
}

UInt CGD_(fdesc_function_line)(FunctionDesc* fdesc) {
	CGD_ASSERT(fdesc != 0);
	return fdesc->fn_line;
}

void CGD_(print_fdesc)(FunctionDesc* fdesc) {
	if (!fdesc)
		VG_(printf)("unknown");
	else
		VG_(printf)("%s::%s(%u)",
				(fdesc->obj_name ? fdesc->obj_name : unknown_name),
				(fdesc->fn_name ? fdesc->fn_name : unknown_name),
				fdesc->fn_line);
}

void CGD_(fprint_fdesc)(VgFile* fp, FunctionDesc* fdesc) {
	CGD_ASSERT(fp != 0);

	if (!fdesc)
		VG_(fprintf)(fp, "unknown");
	else
		VG_(fprintf)(fp, "%s::%s(%u)",
				(fdesc->obj_name ? fdesc->obj_name : unknown_name),
				(fdesc->fn_name ? fdesc->fn_name : unknown_name),
				fdesc->fn_line);
}

HChar* CGD_(fdesc2str)(FunctionDesc* fdesc) {
	const HChar* obj_name;
	const HChar* fn_name;
	HChar* fn_obj_name;

	if (fdesc == NULL) {
		return CGD_STRDUP("cgd.fdesc.fts.1", "unknown");
	} else {
		obj_name = fdesc->obj_name ? fdesc->obj_name : unknown_name;
		fn_name = fdesc->fn_name ? fdesc->fn_name : unknown_name;

		SizeT size = VG_(strlen)(obj_name) + VG_(strlen)(fn_name) + + 3;
		fn_obj_name = CGD_MALLOC("cgd.fts.2", size);
		if (!fn_obj_name)
			VG_(tool_panic)("cfggrind: unable to allocate memory");

		VG_(snprintf)(fn_obj_name, size, "%s::%s\n", obj_name, fn_name);

		return fn_obj_name;
	}
}

FunctionDesc* CGD_(str2fdesc)(const HChar* str) {
	HChar* ptr;
	HChar* tmp;
	SizeT size;
	FunctionDesc* fdesc;

	if (!str || VG_(strcasecmp)(str, "unknown") == 0)
		return 0;

	fdesc = (FunctionDesc*) CGD_MALLOC("cgd.fdesc.s2f.1", sizeof(FunctionDesc));

	ptr = VG_(strrchr)(str, '(');
	if (ptr && (*(ptr + 1) >= '0' && *(ptr + 1) <= '9')) {
		size = (ptr - str) / sizeof(HChar);
		fdesc->obj_name = (HChar*) CGD_MALLOC("cgd.fdesc.s2f.2", ((size + 1) * sizeof(HChar)));
		VG_(strncpy)(fdesc->obj_name, str, size);
		*(fdesc->obj_name + size) = 0;

		ptr++;
		tmp = CGD_STRDUP("cgd.fdesc.s2f.3", ptr);
		if ((ptr = VG_(strchr)(tmp, ')')))
			*ptr = 0;

		fdesc->fn_line = VG_(strtoll10)(tmp, 0);
		CGD_FREE(tmp);
	} else {
		fdesc->obj_name = CGD_STRDUP("cgd.fdesc.s2f.4", str);
		fdesc->fn_line = 0;
	}

	if ((ptr = VG_(strstr)(fdesc->obj_name, "::"))) {
		*ptr = 0;

		ptr += 2;
		fdesc->fn_name = CGD_STRDUP("cgd.fdesc.s2f.5", ptr);
	} else {
		fdesc->fn_name = fdesc->obj_name;
		fdesc->obj_name = 0;
	}

	return fdesc;
}

Bool CGD_(is_main_function)(FunctionDesc* fdesc) {
	return fdesc && fdesc->fn_name && VG_(strcmp)(fdesc->fn_name, main_fname) == 0;
}

Bool CGD_(compare_functions_desc)(FunctionDesc* fdesc1, FunctionDesc* fdesc2) {
	return (fdesc1 && fdesc2 &&
		    !VG_(strcmp)(fdesc1->obj_name, fdesc2->obj_name) &&
		    !VG_(strcmp)(fdesc1->fn_name, fdesc2->fn_name) &&
			fdesc1->fn_line == fdesc2->fn_line);
}
