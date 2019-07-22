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
	LPG_ASSERT(shash->table[index] != 0 && LPG_(smart_list_count)(shash->table[index]) == 1);
//	LPG_ASSERT(!LPG_(smart_list_contains)(shash->track, v, cmp_values));
	LPG_(smart_list_add)(shash->track, v);
}

static
void remove_tracking_value(SmartHash* shash, Int index) {
	Int i, count;

	LPG_ASSERT(shash->table[index] == 0 || LPG_(smart_list_is_empty)(shash->table[index]));

	count = LPG_(smart_list_count)(shash->track);
	for (i = 0; i < count; i++) {
		Int v = ((HWord) LPG_(smart_list_at)(shash->track, i)) - 1;
		if (v == index) {
			LPG_(smart_list_set)(shash->track, i, LPG_(smart_list_at)(shash->track, (count - 1)));
			LPG_(smart_list_set)(shash->track, (count - 1), 0);
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
	LPG_ASSERT(new_size > shash->size);

	new_table = (SmartList**) LPG_MALLOC("lg.smarthash.gsh.1", (new_size * sizeof(SmartList*)));
	VG_(memset)(new_table, 0, (new_size * sizeof(SmartList*)));

#ifdef OPTIMIZED_HASHTABLE
	new_track = LPG_(new_smart_list)(128);

	count = LPG_(smart_list_count)(shash->track);
	for (i = 0; i < count; i++) {
		idx = ((HWord) LPG_(smart_list_at)(shash->track, i)) - 1;
		list = shash->table[idx];
		LPG_ASSERT(list != 0 && !LPG_(smart_list_is_empty)(list));
#else
	LPG_UNUSED(i);
	LPG_UNUSED(count);
	for (idx = 0; idx < shash->size; idx++) {
		list = shash->table[idx];
		if (!list)
			continue;
#endif
		count2 = LPG_(smart_list_count)(list);
		for (j = 0; j < count2; j++) {
			value = LPG_(smart_list_at)(list, j);
			LPG_ASSERT(value != 0);

			key = (*hash_key)(value);
			new_idx = key % new_size;

			new_list = &(new_table[new_idx]);
			if (!*new_list)
				*new_list = LPG_(new_smart_list)(1);

			LPG_(smart_list_add)(*new_list, value);
#ifdef OPTIMIZED_HASHTABLE
			if (LPG_(smart_list_count)(*new_list) == 1)
				LPG_(smart_list_add)(new_track, (void*) (((HWord) new_idx) + 1));
#endif

			LPG_(smart_list_set)(list, j, 0);
		}

		LPG_(delete_smart_list)(list);
	}

	LPG_FREE(shash->table);

	shash->size = new_size;
	shash->table = new_table;

#ifdef OPTIMIZED_HASHTABLE
	LPG_(smart_list_clear)(shash->track, 0);
	LPG_(delete_smart_list)(shash->track);

	shash->track = new_track;
#endif
}

static
SmartHash* create_smart_hash(Int size, Bool fixed) {
	SmartHash* shash;

	LPG_ASSERT(size > 0);

	// Make it 7 multiple.
	size += size % 7;

	shash = (SmartHash*) LPG_MALLOC("lg.smarthash.nsh.1", sizeof(SmartHash));
	VG_(memset)(shash, 0, sizeof(SmartHash));

	shash->size = size;
	shash->fixed = fixed;
	shash->growth_rate = 2.0f; // default: double the hash.

#ifdef OPTIMIZED_HASHTABLE
	shash->track = LPG_(new_smart_list)(128);
#endif

	shash->table = (SmartList**) LPG_MALLOC("lg.smarthash.nsh.2", (size * sizeof(SmartList*)));
	VG_(memset)(shash->table, 0, (size * sizeof(SmartList*)));

	return shash;
}

SmartHash* LPG_(new_smart_hash)(Int size) {
	return create_smart_hash(size, False);
}

SmartHash* LPG_(new_fixed_smart_hash)(Int size) {
	return create_smart_hash(size, True);
}

void LPG_(delete_smart_hash)(SmartHash* shash) {
	Int idx, i, count;
	SmartList* list;

	LPG_ASSERT(shash != 0);
	LPG_ASSERT(shash->count == 0);

#ifdef OPTIMIZED_HASHTABLE
	count = LPG_(smart_list_count)(shash->track);
	for (i = 0; i < count; i++) {
		idx = ((HWord) LPG_(smart_list_at)(shash->track, i)) - 1;
		list = shash->table[idx];
		LPG_ASSERT(list != 0 && !LPG_(smart_list_is_empty)(list));
#else
	LPG_UNUSED(i);
	LPG_UNUSED(count);
	for (idx = 0; idx < shash->size; idx++) {
		list = shash->table[idx];
		if (!list)
			continue;
#endif
		LPG_ASSERT(LPG_(smart_list_is_empty)(list));
		LPG_(delete_smart_list)(list);
	}

#ifdef OPTIMIZED_HASHTABLE
	LPG_ASSERT(LPG_(smart_list_is_empty)(shash->track));
	LPG_(delete_smart_list)(shash->track);
#endif

	LPG_FREE(shash->table);
	LPG_FREE(shash);
}

void LPG_(smart_hash_clear)(SmartHash* shash, void (*remove_value)(void*)) {
	Int idx, i, count, j, count2;
	void* v;
	SmartList* list;

	LPG_ASSERT(shash != 0);

#ifdef OPTIMIZED_HASHTABLE
	count = LPG_(smart_list_count)(shash->track);
	for (i = 0; i < count; i++) {
		idx = ((HWord) LPG_(smart_list_at)(shash->track, i)) - 1;
		list = shash->table[idx];
		LPG_ASSERT(list != 0 && !LPG_(smart_list_is_empty)(list));
#else
	LPG_UNUSED(i);
	LPG_UNUSED(count);
	for (idx = 0; idx < shash->size; idx++) {
		list = shash->table[idx];
		if (!list)
			continue;
#endif
		count2 = LPG_(smart_list_count)(list);
		for (j = 0; j < count2; j++) {
			v = LPG_(smart_list_at)(list, j);
			LPG_ASSERT(v != 0);
			LPG_ASSERT(shash->count > 0);

			if (remove_value)
				(*remove_value)(v);

			--shash->count;
			LPG_(smart_list_set)(list, j, 0);
		}
	}

#ifdef OPTIMIZED_HASHTABLE
	LPG_(smart_list_clear)(shash->track, 0);
#endif
}

Int LPG_(smart_hash_count)(SmartHash* shash) {
	LPG_ASSERT(shash != 0);
	return shash->count;
}

Int LPG_(smart_hash_size)(SmartHash* shash) {
	LPG_ASSERT(shash != 0);
	return shash->size;
}

Bool LPG_(smart_hash_is_empty)(SmartHash* shash) {
	LPG_ASSERT(shash != 0);
	return shash->count == 0;
}

Float LPG_(smart_hash_growth_rate)(SmartHash* shash) {
	LPG_ASSERT(shash != 0);
	return shash->growth_rate;
}

void LPG_(smart_hash_set_growth_rate)(SmartHash* shash, Float rate) {
	LPG_ASSERT(shash != 0);
	LPG_ASSERT(rate > 1.0f);

	shash->growth_rate = rate;
}

void* LPG_(smart_hash_get)(SmartHash* shash, HWord key, HWord (*hash_key)(void*)) {
	Int idx;
	void* v;
	SmartList* list;

	LPG_ASSERT(shash != 0);
	LPG_ASSERT(hash_key != 0);

	idx = key % shash->size;
	if ((list = shash->table[idx])) {
		Int i, count;

		count = LPG_(smart_list_count)(list);
		for (i = 0; i < count; i++) {
			v = LPG_(smart_list_at)(list, i);
			LPG_ASSERT(v != 0);

			if ((*hash_key)(v) == key)
				return v;
		}
	}

	// Not found.
	return 0;
}

void* LPG_(smart_hash_put)(SmartHash* shash, void* value, HWord (*hash_key)(void*)) {
	Int idx;
	HWord key;
	void* v;
	SmartList** list;

	LPG_ASSERT(shash != 0);
	LPG_ASSERT(value != 0);
	LPG_ASSERT(hash_key != 0);

	if (!shash->fixed) {
		if ((10 * shash->count / shash->size) > 6)
			grow_smart_hash(shash, hash_key);
	}

	idx = (*hash_key)(value) % shash->size;
	list = &(shash->table[idx]);

	if (!*list)
		*list = LPG_(new_smart_list)(1);
	else {
		Int i, count;

		key = (*hash_key)(value);
		count = LPG_(smart_list_count)(*list);
		for (i = 0; i < count; i++) {
			v = LPG_(smart_list_at)(*list, i);
			LPG_ASSERT(v != 0);

			if ((*hash_key)(v) == key) {
				// Replace with the new value.
				LPG_(smart_list_set)(*list, i, value);

				// Return the old value.
				return v;
			}
		}
	}

	LPG_(smart_list_add)(*list, value);
#ifdef OPTIMIZED_HASHTABLE
	if (LPG_(smart_list_count)(*list) == 1)
		add_tracking_value(shash, idx);
#endif

	++shash->count;

	return 0;
}

void* LPG_(smart_hash_remove)(SmartHash* shash, HWord key, HWord (*hash_key)(void*)) {
	Int idx;
	void* v;
	SmartList* list;

	LPG_ASSERT(shash != 0);
	LPG_ASSERT(hash_key != 0);

	idx = key % shash->size;
	if ((list = shash->table[idx])) {
		Int i, count;

		count = LPG_(smart_list_count)(list);
		for (i = 0; i < count; i++) {
			v = LPG_(smart_list_at)(list, i);
			LPG_ASSERT(v != 0);

			if ((*hash_key)(v) == key) {
				LPG_(smart_list_set)(list, i, LPG_(smart_list_at)(list, (count - 1)));
				LPG_(smart_list_set)(list, (count - 1), 0);
				--shash->count;

#ifdef OPTIMIZED_HASHTABLE
				if (LPG_(smart_list_is_empty)(list))
					remove_tracking_value(shash, idx);
#endif

				// Return the old value.
				return v;
			}
		}
	}

	return 0;
}

Bool LPG_(smart_hash_contains)(SmartHash* shash, HWord key, HWord (*hash_key)(void*)) {
	return LPG_(smart_hash_get)(shash, key, hash_key) != 0;
}

void LPG_(smart_hash_forall)(SmartHash* shash, Bool (*func)(void*, void*), void* arg) {
	Int idx, i, count, j, count2;
	void* v;
	SmartList* list;

	LPG_ASSERT(shash != 0);
	LPG_ASSERT(func != 0);

#ifdef OPTIMIZED_HASHTABLE
	count = LPG_(smart_list_count)(shash->track);

	i = 0;
	while (i < count) {
		idx = ((HWord) LPG_(smart_list_at)(shash->track, i)) - 1;
		list = shash->table[idx];
		LPG_ASSERT(list != 0 && !LPG_(smart_list_is_empty)(list));
#else
	LPG_UNUSED(i);
	LPG_UNUSED(count);
	for (idx = 0; idx < shash->size; idx++) {
		list = shash->table[idx];
		if (!list)
			continue;
#endif

		count2 = LPG_(smart_list_count)(list);
		for (j = count2 - 1; j >= 0; j--) {
			v = LPG_(smart_list_at)(list, j);
			LPG_ASSERT(v != 0);

			if ((*func)(v, arg)) {
				Int last = LPG_(smart_list_count)(list) - 1;

				if (j != last)
					LPG_(smart_list_set)(list, j, LPG_(smart_list_at)(list, last));

				LPG_(smart_list_set)(list, last, 0);
				--shash->count;
			}
		}

#ifdef OPTIMIZED_HASHTABLE
		if (LPG_(smart_list_is_empty)(list)) {
			LPG_(smart_list_set)(shash->track, i, LPG_(smart_list_at)(shash->track, (count - 1)));
			LPG_(smart_list_set)(shash->track, (count - 1), 0);

			--count;
		} else {
			i++;
		}
#endif
	}
}

// This method moves elements to dst from src (removing them).
void LPG_(smart_hash_merge)(SmartHash* dst, SmartHash* src, HWord (*hash_key)(void*)) {
	Int idx, i, count, j, count2;
	void* v;
	SmartList* list;

	LPG_ASSERT(src != 0);
	LPG_ASSERT(hash_key != 0);

#ifdef OPTIMIZED_HASHTABLE
	count = LPG_(smart_list_count)(src->track);
	for (i = 0; i < count; i++) {
		idx = ((HWord) LPG_(smart_list_at)(src->track, i)) - 1;
		list = src->table[idx];
		LPG_ASSERT(list != 0 && !LPG_(smart_list_is_empty)(list));
#else
	LPG_UNUSED(i);
	LPG_UNUSED(count);
	for (idx = 0; idx < src->size; idx++) {
		list = src->table[idx];
		if (!list)
			continue;
#endif

		count2 = LPG_(smart_list_count)(list);
		for (j = count2 - 1; j >= 0; j--) {
			v = LPG_(smart_list_at)(list, j);
			LPG_ASSERT(v != 0);

			LPG_(smart_list_set)(list, j, 0);
			src->count--;

			LPG_(smart_hash_put)(dst, v, hash_key);
		}
	}

	LPG_ASSERT(LPG_(smart_hash_is_empty)(src));
#ifdef OPTIMIZED_HASHTABLE
	LPG_(smart_list_clear)(src->track, 0);
#endif
}
