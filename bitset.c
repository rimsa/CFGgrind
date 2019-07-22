// Shamefully transcoded from Java's BitSet implementation.

#include "global.h"

#define LONG_MASK 0x3f

#ifdef TRACKING_CELLS
  #define OPTIMIZED_BITSET
#endif

struct _BitSet {
	Int length;
	Bool fixed;
#ifdef OPTIMIZED_BITSET
	SmartList* track;
#endif
	ULong* bits;
};

struct _SmartSeek {
	BitSet *bs;
	Int offset;
	Int pos;
};

#ifdef OPTIMIZED_BITSET
//static
//Bool cmp_values(void* v1, void* v2) {
//	return v1 != 0 && v2 != 0 && v1 == v2;
//}

static
void add_tracking_value(BitSet* bs, Int offset) {
	void* v;

	v = (void*) (((HWord) offset) + 1);
	LPG_ASSERT(bs->bits[offset] != 0);
//	LPG_ASSERT(!LPG_(smart_list_contains)(bs->track, v, cmp_values));
	LPG_(smart_list_add)(bs->track, v);
}

static
void remove_tracking_value(BitSet* bs, Int offset) {
	Int i, count;

	LPG_ASSERT(bs->bits[offset] == 0);

	count = LPG_(smart_list_count)(bs->track);
	for (i = 0; i < count; i++) {
		Int v = ((HWord) LPG_(smart_list_at)(bs->track, i)) - 1;
		if (v == offset) {
			LPG_(smart_list_set)(bs->track, i, LPG_(smart_list_at)(bs->track, (count - 1)));
			LPG_(smart_list_set)(bs->track, (count - 1), 0);
			return;
		}
	}

	tl_assert(0);
}
#endif

#ifdef OPTIMIZED_BITSET
#define BITSET_SET(BS, OFFSET, OP, VALUE) { \
	ULong old_value = BS->bits[OFFSET]; \
	BS->bits[OFFSET] OP VALUE; \
	if (BS->bits[OFFSET]) { \
		if (!old_value) \
			add_tracking_value(BS, OFFSET); \
	} else { \
		if (old_value) \
			remove_tracking_value(BS, OFFSET); \
	} \
}
#else
#define BITSET_SET(BS, OFFSET, OP, VALUE) BS->bits[OFFSET] OP VALUE
#endif

static
void ensure_length(BitSet* bs, Int last_element) {
	if (last_element >= bs->length) {
		if (bs->fixed)
			tl_assert("Not allowed to enlarge this bitset.");

		bs->bits = (ULong*) LPG_REALLOC("lg.bitset.el.1",
				bs->bits, ((last_element + 1) * sizeof(ULong)));
		VG_(memset)((bs->bits + bs->length), 0,
				((last_element + 1 - bs->length) * sizeof(ULong)));
		bs->length = last_element + 1;
	}
}

static
BitSet* create_bitset(Int size, Bool fixed) {
	Int tmp;
	BitSet* bs;

	LPG_ASSERT(size > 0);

	bs = LPG_MALLOC("lg.bitset.nbs.1", sizeof(BitSet));
	VG_(memset)(bs, 0, sizeof(BitSet));

	bs->length = size >> 6;
	if (size & LONG_MASK)
		++bs->length;

	bs->fixed = fixed;

	tmp = bs->length * sizeof(ULong);
	bs->bits = LPG_MALLOC("lg.bitset.nbs.2", tmp);
	VG_(memset)(bs->bits, 0, tmp);

#ifdef OPTIMIZED_BITSET
	bs->track = LPG_(new_smart_list)(128);
#endif

	return bs;
}

BitSet* LPG_(new_bitset)(Int size) {
	return create_bitset(size, False);
}

BitSet* LPG_(new_fixed_bitset)(Int size) {
	return create_bitset(size, True);
}

void LPG_(delete_bitset)(BitSet* bs) {
	LPG_ASSERT(bs != 0);

#ifdef OPTIMIZED_BITSET
	LPG_(smart_list_clear)(bs->track, 0);
	LPG_(delete_smart_list)(bs->track);
#endif

	LPG_FREE(bs->bits);
	LPG_DATA_FREE(bs, sizeof(BitSet));

}

void LPG_(bitset_grow)(BitSet* bs, Int new_size) {
	Int new_length;

	LPG_ASSERT(bs != 0);
	LPG_ASSERT(new_size > 0);

	new_length = new_size >> 6;
	if (new_size & LONG_MASK)
		++new_length;

	ensure_length(bs, new_length-1);
}

void LPG_(bitset_copy)(BitSet* dst, BitSet* src) {
	LPG_ASSERT(dst != 0);
	LPG_ASSERT(src != 0);

	ensure_length(dst, src->length-1);

#ifdef OPTIMIZED_BITSET
	{
		Int i;

		LPG_(smart_list_clear)(dst->track, 0);
		for (i = 0; i < src->length; i++) {
			dst->bits[i] = src->bits[i];
			if (dst->bits[i])
				add_tracking_value(dst, i);
		}

		for (; i < dst->length; i++)
			dst->bits[i] = 0;
	}
#else
	VG_(memcpy)(dst->bits, src->bits, (src->length * sizeof(ULong)));
	if (dst->length > src->length)
		VG_(memset)((dst->bits + src->length), 0, ((dst->length - src->length) * sizeof(ULong)));
#endif
}

BitSet* LPG_(bitset_clone)(BitSet* bs) {
	BitSet* new_bs;

	LPG_ASSERT(bs != 0);

	new_bs = LPG_MALLOC("lg.bitset.bsc.1", sizeof(BitSet));
	VG_(memset)(new_bs, 0, sizeof(BitSet));

	new_bs->length = bs->length;
	new_bs->fixed = bs->fixed;
	new_bs->bits = LPG_MALLOC("lg.bitset.bsc.2", (bs->length * sizeof(ULong)));
	VG_(memcpy)(new_bs->bits, bs->bits, (bs->length * sizeof(ULong)));

#ifdef OPTIMIZED_BITSET
	new_bs->track = LPG_(clone_smart_list)(bs->track);
#endif

	return new_bs;
}

Int LPG_(bitset_size)(BitSet* bs) {
	LPG_ASSERT(bs != 0);
	return bs->length * sizeof(ULong);
}

Int LPG_(bitset_cardinality)(BitSet* bs) {
	Int i, total;

	LPG_ASSERT(bs != 0);

	total = 0;
	for (i = 0; i < bs->length; i++) {
		register ULong tmp = bs->bits[i];
		while (tmp) {
			total += tmp & 1;
			tmp >>= 1;
		}
	}

	return total;
}

Bool LPG_(bitset_cmp)(BitSet* bs1, BitSet* bs2) {
	Int i, j, min;

	LPG_ASSERT(bs1 != 0);
	LPG_ASSERT(bs2 != 0);

	min = VG_MIN(bs1->length, bs2->length);
	for (i = 0; i < min; i++) {
		if (bs1->bits[i] != bs2->bits[i])
			return False;
	}

	for (j = i; j < bs1->length; j++) {
		if (bs1->bits[j])
			return False;
	}

	for (j = i; j < bs2->length; j++) {
		if (bs2->bits[j])
			return False;
	}

	return True;
}

Bool LPG_(bitset_is_empty)(BitSet* bs) {
	LPG_ASSERT(bs != 0);

#ifdef OPTIMIZED_BITSET
	return LPG_(smart_list_count)(bs->track) == 0;
#else
	{
		Int i;

		for (i = 0; i < bs->length; i++) {
			if (bs->bits[i] != 0)
				return False;
		}

		return True;
	}
#endif
}

Bool LPG_(bitset_is_empty_pos)(BitSet* bs, Int pos) {
	return !LPG_(bitset_get_pos)(bs, pos);
}

Bool LPG_(bitset_is_empty_range)(BitSet* bs, Int from, Int to) {
	Int i, low_offset, high_offset;

	LPG_ASSERT(bs != 0);
	LPG_ASSERT(from >= 0 && from <= to);

	if (from == to)
		return True;

#ifdef OPTIMIZED_BITSET
	if (LPG_(smart_list_count)(bs->track) == 0)
		return True;
#endif

	low_offset = from >> 6;
	high_offset = to >> 6;
	ensure_length(bs, high_offset);

	if (low_offset == high_offset)
		return (bs->bits[high_offset] & ((-1L << from) & ((1L << to) - 1))) == 0;
	else {
		if ((bs->bits[low_offset] & (-1L << from)) != 0)
			return False;

		if ((bs->bits[high_offset] & ((1L << to) - 1)) != 0)
			return False;

		for (i = low_offset + 1; i < high_offset; i++) {
			if (bs->bits[i] != 0)
				return False;
		}

		return True;
	}
}

void LPG_(bitset_clear)(BitSet* bs) {
	LPG_ASSERT(bs != 0);

#ifdef OPTIMIZED_BITSET
	{
		Int i, count;
		count = LPG_(smart_list_count)(bs->track);
		for (i = 0; i < count; i++) {
			Int offset = ((HWord) LPG_(smart_list_at)(bs->track, i)) - 1;
			LPG_(smart_list_set)(bs->track, i, 0);
			bs->bits[offset] = 0;
		}
	}
#else
	VG_(memset)(bs->bits, 0, (bs->length * sizeof(ULong)));
#endif
}

void LPG_(bitset_clear_pos)(BitSet* bs, Int pos) {
	Int offset;

	LPG_ASSERT(bs != 0);
	LPG_ASSERT(pos >= 0);

	offset = pos >> 6;
	ensure_length(bs, offset);

	if (bs->bits[offset] == 0)
		return;

	BITSET_SET(bs, offset, &=, ~(1L << pos));
}

void LPG_(bitset_clear_range)(BitSet* bs, Int from, Int to) {
	Int i, low_offset, high_offset;

	LPG_ASSERT(bs != 0);
	LPG_ASSERT(from >= 0 && from <= to);

	if (from == to)
		return;

	low_offset = from >> 6;
	high_offset = to >> 6;
	ensure_length(bs, high_offset);

	if (low_offset == high_offset) {
		BITSET_SET(bs, high_offset, &=, ((1L << from) - 1) | (-1L << to));
	} else {
		BITSET_SET(bs, low_offset, &=, (1L << from) - 1);
		BITSET_SET(bs, high_offset, &=, -1L << to);
		for (i = low_offset + 1; i < high_offset; i++)
			BITSET_SET(bs, i, =, 0);
	}
}

Bool LPG_(bitset_get_pos)(BitSet* bs, Int pos) {
	Int offset;

	LPG_ASSERT(bs != 0);
	LPG_ASSERT(pos >= 0);

	offset = pos >> 6;
	ensure_length(bs, offset);

	return (bs->bits[offset] & (1L << pos)) != 0;
}

BitSet* LPG_(bitset_get_range)(BitSet* bs, Int from, Int to) {
	Int i, len, reverse, low_offset, high_offset, low_bit;
	BitSet* new_bs;

	LPG_ASSERT(bs != 0);
	LPG_ASSERT(from >= 0 && from <= to);

	new_bs = create_bitset(to - from, bs->fixed);
	low_offset = from >> 6;
	if (low_offset >= bs->length || to == from)
		return new_bs;

	low_bit = from & LONG_MASK;
	high_offset = to >> 6;
	if (low_bit == 0) {
		len = VG_MIN(high_offset - low_offset + 1, bs->length - low_offset);

#ifdef OPTIMIZED_BITSET
		for (i = 0; i < len; i++)
			BITSET_SET(new_bs, i, =, bs->bits[low_offset + i]);
#else
		VG_(memcpy)(new_bs->bits, (bs->bits + low_offset), (len * sizeof(ULong)));
#endif

		if (high_offset < bs->length)
			BITSET_SET(new_bs, high_offset - low_offset, &=, (1L << to) - 1);

		return new_bs;
	}

	len = VG_MIN(high_offset, bs->length - 1);
	reverse = 64 - low_bit;

	for (i = 0; low_offset < len; low_offset++, i++) {
		BITSET_SET(new_bs, i, =, ((bs->bits[low_offset] >> low_bit)
							| (bs->bits[low_offset + 1] << reverse)));
	}

	if ((to & LONG_MASK) > low_bit)
		BITSET_SET(new_bs, i++, =, bs->bits[low_offset] >> low_bit);

	if (high_offset < bs->length)
		BITSET_SET(new_bs, i - 1, &=, (1L << (to - from)) - 1);

	 return new_bs;
}

void LPG_(bitset_set_pos)(BitSet* bs, Int pos) {
	Int offset;

	LPG_ASSERT(bs != 0);
	LPG_ASSERT(pos >= 0);

	offset = pos >> 6;
	ensure_length(bs, offset);

	BITSET_SET(bs, offset, |=,  1L << pos);
}

void LPG_(bitset_set_pos_value)(BitSet* bs, Int pos, Bool value) {
	if (value)
		LPG_(bitset_set_pos)(bs, pos);
	else
		LPG_(bitset_clear_pos)(bs, pos);
}

void LPG_(bitset_set_range)(BitSet* bs, Int from, Int to) {
	Int i, low_offset, high_offset;

	LPG_ASSERT(bs != 0);
	LPG_ASSERT(from >= 0 && from <= to);

	if (from == to)
		return;

	low_offset = from >> 6;
	high_offset = to >> 6;
	ensure_length(bs, high_offset);

	if (low_offset == high_offset) {
		BITSET_SET(bs, high_offset, |=, (-1L << from) & ((1L << to) - 1));
	} else {
		BITSET_SET(bs, low_offset, |=, -1L << from);
		BITSET_SET(bs, high_offset, |=, (1L << to) - 1);
		for (i = low_offset + 1; i < high_offset; i++)
			BITSET_SET(bs, i, =, -1L);
	}
}

void LPG_(bitset_set_range_value)(BitSet* bs, Int from, Int to, Bool value) {
	if (value)
		LPG_(bitset_set_range)(bs, from, to);
	else
		LPG_(bitset_clear_range)(bs, from, to);
}

void LPG_(bitset_flip)(BitSet* bs) {
	LPG_(bitset_not)(bs);
}

void LPG_(bitset_flip_pos)(BitSet* bs, Int pos) {
	Int offset;

	LPG_ASSERT(bs != 0);
	LPG_ASSERT(pos >= 0);

	offset = pos >> 6;
	ensure_length(bs, offset);

	BITSET_SET(bs, offset, ^=, 1L << pos);
}

void LPG_(bitset_flip_range)(BitSet* bs, Int from, Int to) {
	Int i, low_offset, high_offset;

	LPG_ASSERT(bs != 0);
	LPG_ASSERT(from >= 0 && from <= to);

	if (from == to)
		return;

	low_offset = from >> 6;
	high_offset = to >> 6;
	ensure_length(bs, high_offset);

	if (low_offset == high_offset) {
		BITSET_SET(bs, high_offset, ^=, (-1L << from) & ((1L << to) - 1));
	} else {
		BITSET_SET(bs, low_offset, ^=, -1L << from);
		BITSET_SET(bs, high_offset, ^=, (1L << to) - 1);
		for (i = low_offset + 1; i < high_offset; i++)
			BITSET_SET(bs, i, ^=, -1);
	}
}

void LPG_(bitset_not)(BitSet* bs) {
	Int i;

	LPG_ASSERT(bs != 0);

	for (i = 0; i < bs->length; i++)
		BITSET_SET(bs, i, =, ~bs->bits[i]);
}

void LPG_(bitset_and)(BitSet* dst, BitSet* src) {
	Int i, min;

	LPG_ASSERT(dst != 0);
	LPG_ASSERT(src != 0);

	min = VG_MIN(dst->length, src->length);
	for (i = 0; i < min; i++)
		BITSET_SET(dst, i, &=, src->bits[i]);

	for (; i < dst->length; i++) {
		BITSET_SET(dst, i, =, 0);
	}
}

void LPG_(bitset_or)(BitSet* dst, BitSet* src) {
	Int i;

	LPG_ASSERT(dst != 0);
	LPG_ASSERT(src != 0);

	ensure_length(dst, src->length-1);
	for (i = 0; i < src->length; i++) {
		BITSET_SET(dst, i, |=, src->bits[i]);
	}
}

void LPG_(bitset_xor)(BitSet* dst, BitSet* src) {
	Int i;

	ensure_length(dst, src->length-1);
	for (i = 0; i < src->length; i++) {
		BITSET_SET(dst, i, ^=, src->bits[i]);
	}
}

void LPG_(bitset_and_not)(BitSet* dst, BitSet* src) {
	Int i, min;

	LPG_ASSERT(dst != 0);
	LPG_ASSERT(src != 0);

	min = VG_MIN(dst->length, src->length);
	for (i = 0; i < min; i++) {
		BITSET_SET(dst, i, &=, ~src->bits[i]);
	}
}

void LPG_(bitset_forall_set)(BitSet* bs, Bool (*func)(Int, void*), void* arg) {
	Int i, count, offset, pos;
	register ULong tmp;

	LPG_ASSERT(bs != 0);

#ifdef OPTIMIZED_BITSET
	i = 0;
	count = LPG_(smart_list_count)(bs->track);
	while (i < count) {
		offset = ((HWord) LPG_(smart_list_at)(bs->track, i)) - 1;
		LPG_ASSERT(bs->bits[offset] != 0);
#else
	LPG_UNUSED(i);
	LPG_UNUSED(count);
	for (offset = 0; offset < bs->length; offset++) {
#endif
		pos = offset * 64;
		tmp = bs->bits[offset];
		while (tmp) {
			if (tmp & 1) {
				if ((*func)(pos, arg))
					bs->bits[offset] &= ~(1L << pos);
			}

			tmp >>= 1;
			++pos;
		}

#ifdef OPTIMIZED_BITSET
		if (bs->bits[offset] == 0) {
			LPG_(smart_list_set)(bs->track, i, LPG_(smart_list_at)(bs->track, (count - 1)));
			LPG_(smart_list_set)(bs->track, (count - 1), 0);

			--count;
		} else {
			i++;
		}
#endif
	}
}

static
Bool print_pos(Int pos, void* arg) {
	LPG_UNUSED(arg);
	VG_(printf)(" %d", pos);

	return False;
}

void LPG_(bitset_print)(BitSet* bs) {
	LPG_ASSERT(bs != 0);

	VG_(printf)("[");
	LPG_(bitset_forall_set)(bs, print_pos, 0);
	VG_(printf)(" ]");
}

SmartSeek* LPG_(bitset_seek)(BitSet* bs) {
	SmartSeek* ss;

	ss = (SmartSeek*) LPG_MALLOC("lg.bitset.bss.1", sizeof(SmartSeek));
	VG_(memset)(ss, 0, sizeof(SmartSeek));
	ss->bs = bs;

	return ss;
}

void LPG_(bitset_delete_seek)(SmartSeek* ss) {
	LPG_ASSERT(ss != 0);
	LPG_DATA_FREE(ss, sizeof(SmartSeek));
}

void LPG_(bitset_rewind)(SmartSeek* ss) {
	LPG_ASSERT(ss != 0);

	ss->offset = 0;
	ss->pos = 0;
}

Int LPG_(bitset_get_index)(SmartSeek* ss) {
	LPG_ASSERT(ss != 0);
	LPG_ASSERT(ss->offset >= 0 && ss->offset < ss->bs->length);
	LPG_ASSERT(ss->pos >= 0 && ss->pos <= LONG_MASK);

	return (ss->offset * 64 + ss->pos);
}

void LPG_(bitset_set_index)(SmartSeek* ss, Int index) {
	LPG_ASSERT(ss != 0);
	LPG_ASSERT(index >= 0 && index < (ss->bs->length * 64));

	ss->offset = index / 64;
	ss->pos = index % 64;
}

void LPG_(bitset_clear_value)(SmartSeek* ss) {
	LPG_ASSERT(ss != 0);
	LPG_ASSERT(ss->offset >= 0 && ss->offset < ss->bs->length);
	LPG_ASSERT(ss->pos >= 0 && ss->pos <= LONG_MASK);

	BITSET_SET(ss->bs, ss->offset, &=, ~(1L << ss->pos));
}

Bool LPG_(bitset_get)(SmartSeek* ss) {
	LPG_ASSERT(ss != 0);
	LPG_ASSERT(ss->offset >= 0 && ss->offset < ss->bs->length);
	LPG_ASSERT(ss->pos >= 0 && ss->pos <= LONG_MASK);

	return (ss->bs->bits[ss->offset] & (1L << ss->pos)) != 0;
}

void LPG_(bitset_set)(SmartSeek* ss) {
	LPG_ASSERT(ss != 0);
	LPG_ASSERT(ss->offset >= 0 && ss->offset < ss->bs->length);
	LPG_ASSERT(ss->pos >= 0 && ss->pos <= LONG_MASK);

	BITSET_SET(ss->bs, ss->offset, |=, 1L << ss->pos);
}

void LPG_(bitset_set_value)(SmartSeek* ss, Bool value) {
	if (value)
		LPG_(bitset_set)(ss);
	else
		LPG_(bitset_clear_value)(ss);
}

static
Bool seek_until_valid(SmartSeek* ss) {
	LPG_ASSERT(ss != 0);
	LPG_ASSERT(ss->offset >= 0);
	LPG_ASSERT(ss->pos >= 0 && ss->pos <= LONG_MASK);

	while (ss->offset < ss->bs->length) {
		register ULong tmp = ss->bs->bits[ss->offset] >> ss->pos;
		while (tmp) {
			LPG_ASSERT(ss->pos <= LONG_MASK);

			if (tmp & 1)
				return True;

			tmp >>= 1;
			ss->pos++;
		}

		ss->offset++;
		ss->pos = 0;
	}

	return False;
}

Bool LPG_(bitset_has_next)(SmartSeek* ss) {
	return seek_until_valid(ss);
}

void LPG_(bitset_next)(SmartSeek* ss) {
	LPG_ASSERT(ss != 0);
	LPG_ASSERT(ss->offset >= 0);
	LPG_ASSERT(ss->pos >= 0 && ss->pos <= LONG_MASK);

	if (ss->offset < ss->bs->length) {
		ss->pos++;
		if (ss->pos > LONG_MASK) {
			ss->offset++;
			ss->pos = 0;
		}
	}

	seek_until_valid(ss);
}
