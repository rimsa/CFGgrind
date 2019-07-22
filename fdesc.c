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

FunctionDesc* LPG_(new_fdesc)(Addr addr, Bool entry) {
	const HChar* tmp;
	FunctionDesc* fdesc;
	Bool found;

	DiEpoch ep = VG_(current_DiEpoch)();
	found = entry ?
			VG_(get_fnname_if_entry)(ep, addr, &tmp) :
			VG_(get_fnname)(ep, addr, &tmp);

	if (found) {
		fdesc = (FunctionDesc*) LPG_MALLOC("lg.fdesc.nf.1", sizeof(FunctionDesc));

		fdesc->fn_name = VG_(strdup)("lg.fdesc.nf.2", tmp);
		if (!VG_(get_linenum)(ep, addr, &(fdesc->fn_line)))
			fdesc->fn_line = -1;

		fdesc->obj_name = VG_(get_objname)(ep, addr, &tmp) ?
								VG_(strdup)("lg.fdesc.nf.3", tmp) : 0;
	} else {
		fdesc = 0;
	}

	return fdesc;
}

void LPG_(delete_fdesc)(FunctionDesc* fdesc) {
	LPG_ASSERT(fdesc);
	LPG_ASSERT(fdesc->fn_name);

	LPG_DATA_FREE(fdesc->fn_name, VG_(strlen)(fdesc->fn_name));
	if (fdesc->obj_name)
		LPG_DATA_FREE(fdesc->obj_name, VG_(strlen)(fdesc->obj_name));

	LPG_DATA_FREE(fdesc, sizeof(FunctionDesc));
}

HChar* LPG_(fdesc_object_name)(FunctionDesc* fdesc) {
	LPG_ASSERT(fdesc != 0);
	return fdesc->obj_name;
}

HChar* LPG_(fdesc_function_name)(FunctionDesc* fdesc) {
	LPG_ASSERT(fdesc != 0);
	return fdesc->fn_name;
}

UInt LPG_(fdesc_function_line)(FunctionDesc* fdesc) {
	LPG_ASSERT(fdesc != 0);
	return fdesc->fn_line;
}

void LPG_(print_fdesc)(FunctionDesc* fdesc) {
	if (!fdesc)
		VG_(printf)("unknown");
	else
		VG_(printf)("%s::%s(%d)",
				(fdesc->obj_name ? fdesc->obj_name : unknown_name),
				(fdesc->fn_name ? fdesc->fn_name : unknown_name),
				fdesc->fn_line);
}

void LPG_(fprint_fdesc)(VgFile* fp, FunctionDesc* fdesc) {
	LPG_ASSERT(fp != 0);

	if (!fdesc)
		VG_(fprintf)(fp, "unknown");
	else
		VG_(fprintf)(fp, "%s::%s(%d)",
				(fdesc->obj_name ? fdesc->obj_name : unknown_name),
				(fdesc->fn_name ? fdesc->fn_name : unknown_name),
				fdesc->fn_line);
}

HChar* LPG_(fdesc2str)(FunctionDesc* fdesc) {
	const HChar* obj_name;
	const HChar* fn_name;
	HChar* fn_obj_name;

	if (fdesc == NULL) {
		return VG_(strdup)("lg.fdesc.fts.1", "unknown");
	} else {
		obj_name = fdesc->obj_name ? fdesc->obj_name : unknown_name;
		fn_name = fdesc->fn_name ? fdesc->fn_name : unknown_name;

		SizeT size = VG_(strlen)(obj_name) + VG_(strlen)(fn_name) + + 3;
		fn_obj_name = LPG_MALLOC("lg.fts.2", size);
		if (!fn_obj_name)
			VG_(tool_panic)("cfggrind: unable to allocate memory");

		VG_(snprintf)(fn_obj_name, size, "%s::%s\n", obj_name, fn_name);

		return fn_obj_name;
	}
}

FunctionDesc* LPG_(str2fdesc)(const HChar* str) {
	HChar* ptr;
	HChar* tmp;
	SizeT size;
	FunctionDesc* fdesc;

	if (!str || VG_(strcasecmp)(str, "unknown") == 0)
		return 0;

	fdesc = (FunctionDesc*) LPG_MALLOC("lg.fdesc.s2f.1", sizeof(FunctionDesc));

	ptr = VG_(strrchr)(str, '(');
	if (ptr && (*(ptr + 1) >= '0' && *(ptr + 1) <= '9')) {
		size = (ptr - str) / sizeof(HChar);
		fdesc->obj_name = (HChar*) LPG_MALLOC("lg.fdesc.s2f.2", ((size + 1) * sizeof(HChar)));
		VG_(strncpy)(fdesc->obj_name, str, size);
		*(fdesc->obj_name + size) = 0;

		ptr++;
		tmp = VG_(strdup)("lg.fdesc.s2f.3", ptr);
		if ((ptr = VG_(strchr)(tmp, ')')))
			*ptr = 0;

		fdesc->fn_line = VG_(strtoll10)(tmp, 0);
		VG_(free)(tmp);
	} else {
		fdesc->obj_name = VG_(strdup)("lg.fdesc.s2f.4", str);
		fdesc->fn_line = -1;
	}

	if ((ptr = VG_(strstr)(fdesc->obj_name, "::"))) {
		*ptr = 0;

		ptr += 2;
		fdesc->fn_name = VG_(strdup)("lg.fdesc.s2f.5", ptr);
	} else {
		fdesc->fn_name = fdesc->obj_name;
		fdesc->obj_name = 0;
	}

	return fdesc;
}

Bool LPG_(is_main_function)(FunctionDesc* fdesc) {
	return fdesc && fdesc->fn_name && VG_(strcmp)(fdesc->fn_name, main_fname) == 0;
}

Bool LPG_(compare_functions_desc)(FunctionDesc* fdesc1, FunctionDesc* fdesc2) {
	return (fdesc1 && fdesc2 &&
		    !VG_(strcmp)(fdesc1->obj_name, fdesc2->obj_name) &&
		    !VG_(strcmp)(fdesc1->fn_name, fdesc2->fn_name) &&
			fdesc1->fn_line == fdesc2->fn_line);
}
