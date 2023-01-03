/*--------------------------------------------------------------------*/
/*--- CFGgrind                                                     ---*/
/*---                                                  smarthash.c ---*/
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

#ifdef TRACKING_CELLS
  #define OPTIMIZED_HASHTABLE
#endif

struct _SmartHash {
	Int count;
	Int size;
	Bool fixed;
	Float growth_rate;
#ifdef OPTIMIZED_HASHTABLE
	SmartList* track;
#endif
	SmartList** table; // SmartList<void*>
};

#ifdef OPTIMIZED_HASHTABLE
//static
//Bool cmp_values(void* v1, void* v2) {
//	return v1 != 0 && v2 != 0 && v1 == v2;
//}

static
void add_tracking_value(SmartHash* shash, Int index) {
	void* v;

	v = (void*) (((HWord) index) + 1);
	CGD_ASSERT(shash->table[index] != 0 && CGD_(smart_list_count)(shash->table[index]) == 1);
//	CGD_ASSERT(!CGD_(smart_list_contains)(shash->track, v, cmp_values));
	CGD_(smart_list_add)(shash->track, v);
}

static
void remove_tracking_value(SmartHash* shash, Int index) {
	Int i, count;

	CGD_ASSERT(shash->table[index] == 0 || CGD_(smart_list_is_empty)(shash->table[index]));

	count = CGD_(smart_list_count)(shash->track);
	for (i = 0; i < count; i++) {
		Int v = ((HWord) CGD_(smart_list_at)(shash->track, i)) - 1;
		if (v == index) {
			CGD_(smart_list_set)(shash->track, i, CGD_(smart_list_at)(shash->track, (count - 1)));
			CGD_(smart_list_set)(shash->track, (count - 1), 0);
			return;
		}
	}

	tl_assert(0);
}
#endif

static
void grow_smart_hash(SmartHash* shash, HWord (*hash_key)(void*)) {
	Int idx, new_idx, new_size;
	Int i, count, j, count2;
	HWord key;
	void* value;
	SmartList* list;
	SmartList** new_table;
	SmartList** new_list;
#ifdef OPTIMIZED_HASHTABLE
	SmartList* new_track;
#endif

	new_size = (Int) (shash->size * shash->growth_rate);
	CGD_ASSERT(new_size > shash->size);

	new_table = (SmartList**) CGD_MALLOC("cgd.smarthash.gsh.1", (new_size * sizeof(SmartList*)));
	VG_(memset)(new_table, 0, (new_size * sizeof(SmartList*)));

#ifdef OPTIMIZED_HASHTABLE
	new_track = CGD_(new_smart_list)(128);

	count = CGD_(smart_list_count)(shash->track);
	for (i = 0; i < count; i++) {
		idx = ((HWord) CGD_(smart_list_at)(shash->track, i)) - 1;
		list = shash->table[idx];
		CGD_ASSERT(list != 0 && !CGD_(smart_list_is_empty)(list));
#else
	CGD_UNUSED(i);
	CGD_UNUSED(count);
	for (idx = 0; idx < shash->size; idx++) {
		list = shash->table[idx];
		if (!list)
			continue;
#endif
		count2 = CGD_(smart_list_count)(list);
		for (j = 0; j < count2; j++) {
			value = CGD_(smart_list_at)(list, j);
			CGD_ASSERT(value != 0);

			key = (*hash_key)(value);
			new_idx = key % new_size;

			new_list = &(new_table[new_idx]);
			if (!*new_list)
				*new_list = CGD_(new_smart_list)(1);

			CGD_(smart_list_add)(*new_list, value);
#ifdef OPTIMIZED_HASHTABLE
			if (CGD_(smart_list_count)(*new_list) == 1)
				CGD_(smart_list_add)(new_track, (void*) (((HWord) new_idx) + 1));
#endif

			CGD_(smart_list_set)(list, j, 0);
		}

		CGD_(delete_smart_list)(list);
	}

	CGD_FREE(shash->table);

	shash->size = new_size;
	shash->table = new_table;

#ifdef OPTIMIZED_HASHTABLE
	CGD_(smart_list_clear)(shash->track, 0);
	CGD_(delete_smart_list)(shash->track);

	shash->track = new_track;
#endif
}

static
SmartHash* create_smart_hash(Int size, Bool fixed) {
	SmartHash* shash;

	CGD_ASSERT(size > 0);

	// Make it 7 multiple.
	size += size % 7;

	shash = (SmartHash*) CGD_MALLOC("cgd.smarthash.nsh.1", sizeof(SmartHash));
	VG_(memset)(shash, 0, sizeof(SmartHash));

	shash->size = size;
	shash->fixed = fixed;
	shash->growth_rate = 2.0f; // default: double the hash.

#ifdef OPTIMIZED_HASHTABLE
	shash->track = CGD_(new_smart_list)(128);
#endif

	shash->table = (SmartList**) CGD_MALLOC("cgd.smarthash.nsh.2", (size * sizeof(SmartList*)));
	VG_(memset)(shash->table, 0, (size * sizeof(SmartList*)));

	return shash;
}

SmartHash* CGD_(new_smart_hash)(Int size) {
	return create_smart_hash(size, False);
}

SmartHash* CGD_(new_fixed_smart_hash)(Int size) {
	return create_smart_hash(size, True);
}

void CGD_(delete_smart_hash)(SmartHash* shash) {
	Int idx, i, count;
	SmartList* list;

	CGD_ASSERT(shash != 0);
	CGD_ASSERT(shash->count == 0);

#ifdef OPTIMIZED_HASHTABLE
	count = CGD_(smart_list_count)(shash->track);
	for (i = 0; i < count; i++) {
		idx = ((HWord) CGD_(smart_list_at)(shash->track, i)) - 1;
		list = shash->table[idx];
		CGD_ASSERT(list != 0 && !CGD_(smart_list_is_empty)(list));
#else
	CGD_UNUSED(i);
	CGD_UNUSED(count);
	for (idx = 0; idx < shash->size; idx++) {
		list = shash->table[idx];
		if (!list)
			continue;
#endif
		CGD_ASSERT(CGD_(smart_list_is_empty)(list));
		CGD_(delete_smart_list)(list);
	}

#ifdef OPTIMIZED_HASHTABLE
	CGD_ASSERT(CGD_(smart_list_is_empty)(shash->track));
	CGD_(delete_smart_list)(shash->track);
#endif

	CGD_FREE(shash->table);
	CGD_FREE(shash);
}

void CGD_(smart_hash_clear)(SmartHash* shash, void (*remove_value)(void*)) {
	Int idx, i, count, j, count2;
	void* v;
	SmartList* list;

	CGD_ASSERT(shash != 0);

#ifdef OPTIMIZED_HASHTABLE
	count = CGD_(smart_list_count)(shash->track);
	for (i = 0; i < count; i++) {
		idx = ((HWord) CGD_(smart_list_at)(shash->track, i)) - 1;
		list = shash->table[idx];
		CGD_ASSERT(list != 0 && !CGD_(smart_list_is_empty)(list));
#else
	CGD_UNUSED(i);
	CGD_UNUSED(count);
	for (idx = 0; idx < shash->size; idx++) {
		list = shash->table[idx];
		if (!list)
			continue;
#endif
		count2 = CGD_(smart_list_count)(list);
		for (j = 0; j < count2; j++) {
			v = CGD_(smart_list_at)(list, j);
			CGD_ASSERT(v != 0);
			CGD_ASSERT(shash->count > 0);

			if (remove_value)
				(*remove_value)(v);

			--shash->count;
			CGD_(smart_list_set)(list, j, 0);
		}
	}

#ifdef OPTIMIZED_HASHTABLE
	CGD_(smart_list_clear)(shash->track, 0);
#endif
}

Int CGD_(smart_hash_count)(SmartHash* shash) {
	CGD_ASSERT(shash != 0);
	return shash->count;
}

Int CGD_(smart_hash_size)(SmartHash* shash) {
	CGD_ASSERT(shash != 0);
	return shash->size;
}

Bool CGD_(smart_hash_is_empty)(SmartHash* shash) {
	CGD_ASSERT(shash != 0);
	return shash->count == 0;
}

Float CGD_(smart_hash_growth_rate)(SmartHash* shash) {
	CGD_ASSERT(shash != 0);
	return shash->growth_rate;
}

void CGD_(smart_hash_set_growth_rate)(SmartHash* shash, Float rate) {
	CGD_ASSERT(shash != 0);
	CGD_ASSERT(rate > 1.0f);

	shash->growth_rate = rate;
}

void* CGD_(smart_hash_get)(SmartHash* shash, HWord key, HWord (*hash_key)(void*)) {
	Int idx;
	void* v;
	SmartList* list;

	CGD_ASSERT(shash != 0);
	CGD_ASSERT(hash_key != 0);

	idx = key % shash->size;
	if ((list = shash->table[idx])) {
		Int i, count;

		count = CGD_(smart_list_count)(list);
		for (i = 0; i < count; i++) {
			v = CGD_(smart_list_at)(list, i);
			CGD_ASSERT(v != 0);

			if ((*hash_key)(v) == key)
				return v;
		}
	}

	// Not found.
	return 0;
}

void* CGD_(smart_hash_put)(SmartHash* shash, void* value, HWord (*hash_key)(void*)) {
	Int idx;
	HWord key;
	void* v;
	SmartList** list;

	CGD_ASSERT(shash != 0);
	CGD_ASSERT(value != 0);
	CGD_ASSERT(hash_key != 0);

	if (!shash->fixed) {
		if ((10 * shash->count / shash->size) > 6)
			grow_smart_hash(shash, hash_key);
	}

	idx = (*hash_key)(value) % shash->size;
	list = &(shash->table[idx]);

	if (!*list)
		*list = CGD_(new_smart_list)(1);
	else {
		Int i, count;

		key = (*hash_key)(value);
		count = CGD_(smart_list_count)(*list);
		for (i = 0; i < count; i++) {
			v = CGD_(smart_list_at)(*list, i);
			CGD_ASSERT(v != 0);

			if ((*hash_key)(v) == key) {
				// Replace with the new value.
				CGD_(smart_list_set)(*list, i, value);

				// Return the old value.
				return v;
			}
		}
	}

	CGD_(smart_list_add)(*list, value);
#ifdef OPTIMIZED_HASHTABLE
	if (CGD_(smart_list_count)(*list) == 1)
		add_tracking_value(shash, idx);
#endif

	++shash->count;

	return 0;
}

void* CGD_(smart_hash_remove)(SmartHash* shash, HWord key, HWord (*hash_key)(void*)) {
	Int idx;
	void* v;
	SmartList* list;

	CGD_ASSERT(shash != 0);
	CGD_ASSERT(hash_key != 0);

	idx = key % shash->size;
	if ((list = shash->table[idx])) {
		Int i, count;

		count = CGD_(smart_list_count)(list);
		for (i = 0; i < count; i++) {
			v = CGD_(smart_list_at)(list, i);
			CGD_ASSERT(v != 0);

			if ((*hash_key)(v) == key) {
				CGD_(smart_list_set)(list, i, CGD_(smart_list_at)(list, (count - 1)));
				CGD_(smart_list_set)(list, (count - 1), 0);
				--shash->count;

#ifdef OPTIMIZED_HASHTABLE
				if (CGD_(smart_list_is_empty)(list))
					remove_tracking_value(shash, idx);
#endif

				// Return the old value.
				return v;
			}
		}
	}

	return 0;
}

Bool CGD_(smart_hash_contains)(SmartHash* shash, HWord key, HWord (*hash_key)(void*)) {
	return CGD_(smart_hash_get)(shash, key, hash_key) != 0;
}

void CGD_(smart_hash_forall)(SmartHash* shash, Bool (*func)(void*, void*), void* arg) {
	Int idx, i, count, j, count2;
	void* v;
	SmartList* list;

	CGD_ASSERT(shash != 0);
	CGD_ASSERT(func != 0);

#ifdef OPTIMIZED_HASHTABLE
	count = CGD_(smart_list_count)(shash->track);

	i = 0;
	while (i < count) {
		idx = ((HWord) CGD_(smart_list_at)(shash->track, i)) - 1;
		list = shash->table[idx];
		CGD_ASSERT(list != 0 && !CGD_(smart_list_is_empty)(list));
#else
	CGD_UNUSED(i);
	CGD_UNUSED(count);
	for (idx = 0; idx < shash->size; idx++) {
		list = shash->table[idx];
		if (!list)
			continue;
#endif

		count2 = CGD_(smart_list_count)(list);
		for (j = count2 - 1; j >= 0; j--) {
			v = CGD_(smart_list_at)(list, j);
			CGD_ASSERT(v != 0);

			if ((*func)(v, arg)) {
				Int last = CGD_(smart_list_count)(list) - 1;

				if (j != last)
					CGD_(smart_list_set)(list, j, CGD_(smart_list_at)(list, last));

				CGD_(smart_list_set)(list, last, 0);
				--shash->count;
			}
		}

#ifdef OPTIMIZED_HASHTABLE
		if (CGD_(smart_list_is_empty)(list)) {
			CGD_(smart_list_set)(shash->track, i, CGD_(smart_list_at)(shash->track, (count - 1)));
			CGD_(smart_list_set)(shash->track, (count - 1), 0);

			--count;
		} else {
			i++;
		}
#endif
	}
}

// This method moves elements to dst from src (removing them).
void CGD_(smart_hash_merge)(SmartHash* dst, SmartHash* src, HWord (*hash_key)(void*)) {
	Int idx, i, count, j, count2;
	void* v;
	SmartList* list;

	CGD_ASSERT(src != 0);
	CGD_ASSERT(hash_key != 0);

#ifdef OPTIMIZED_HASHTABLE
	count = CGD_(smart_list_count)(src->track);
	for (i = 0; i < count; i++) {
		idx = ((HWord) CGD_(smart_list_at)(src->track, i)) - 1;
		list = src->table[idx];
		CGD_ASSERT(list != 0 && !CGD_(smart_list_is_empty)(list));
#else
	CGD_UNUSED(i);
	CGD_UNUSED(count);
	for (idx = 0; idx < src->size; idx++) {
		list = src->table[idx];
		if (!list)
			continue;
#endif

		count2 = CGD_(smart_list_count)(list);
		for (j = count2 - 1; j >= 0; j--) {
			v = CGD_(smart_list_at)(list, j);
			CGD_ASSERT(v != 0);

			CGD_(smart_list_set)(list, j, 0);
			src->count--;

			CGD_(smart_hash_put)(dst, v, hash_key);
		}
	}

	CGD_ASSERT(CGD_(smart_hash_is_empty)(src));
#ifdef OPTIMIZED_HASHTABLE
	CGD_(smart_list_clear)(src->track, 0);
#endif
}
