/*--------------------------------------------------------------------*/
/*--- Callgrind data structures, functions.               global.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2017 Josef Weidendorfer
      josef.weidendorfer@gmx.de

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

#ifndef LPG_GLOBAL
#define LPG_GLOBAL

#include "pub_tool_basics.h"
#include "pub_tool_vki.h"
#include "pub_tool_debuginfo.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcfile.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_libcproc.h"
#include "pub_tool_machine.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_options.h"
#include "pub_tool_tooliface.h"
#include "pub_tool_xarray.h"
#include "pub_tool_clientstate.h"
#include "pub_tool_machine.h"      // VG_(fnptr_to_fnentry)


#define LPG_(str) VGAPPEND(vgcfggrind_,str)

/*------------------------------------------------------------*/
/*--- Callgrind compile options                           --- */
/*------------------------------------------------------------*/

/* Enable debug output */
#define LPG_ENABLE_DEBUG	1
#define LPG_DEBUG_MEM		0

#define LPG_ENABLE_PATH_CACHE

/* Syscall Timing in microseconds? 
 * (define to 0 if you get compile errors) */
#define LPG_MICROSYSTIME 0

// Chain Smart List: 1
// Realloc Smart List: 2
#define SMART_LIST_MODE 2

// Enable or disable data structure cells tracking
// #define TRACKING_CELLS

/*------------------------------------------------------------*/
/*--- Command line options                                 ---*/
/*------------------------------------------------------------*/

#define DEFAULT_CFGOUTPUT   "cfg.out.%p"

typedef struct _SmartList			SmartList;
typedef struct _CommandLineOptions	CommandLineOptions;
struct _CommandLineOptions {
  /* CFG options */
  const HChar* cfg_outfile;
  const HChar* cfg_infile;
  Bool ignore_failed;       /* Ignored failed CFG read */
  struct {
	  Bool all;             /* Dump all cfgs */
	  SmartList* addrs;     /* List of cfg's addresses to dump */
	  SmartList* fnames;    /* List of cfg's function names to dump */
	  const HChar* dir;     /* Directory where to dump the cfg's */
  } dump_cfgs;
  const HChar* instrs_map;  /* Instructions map file */

#if LPG_ENABLE_DEBUG
  Int   verbose;
  ULong verbose_start;
#endif
};

/*------------------------------------------------------------*/
/*--- Constants                                            ---*/
/*------------------------------------------------------------*/

/* Minimum cache line size allowed */
#define MIN_LINE_SIZE   16

#define MAX_DSIZE    512

/*------------------------------------------------------------*/
/*--- Statistics                                           ---*/
/*------------------------------------------------------------*/

typedef struct _Statistics Statistics;
struct _Statistics {
  ULong bb_executions;

  Int  context_counter;
  Int  bb_retranslations;  

  Int  distinct_objs;
  Int  distinct_files;
  Int  distinct_fns;
  Int  distinct_contexts;
  Int  distinct_bbs;
  Int  distinct_bbccs;
  Int  distinct_instrs;
  Int  distinct_groups;
  Int  distinct_cfgs;
  Int  distinct_cfg_nodes;

  Int  bb_hash_resizes;
  Int  bbcc_hash_resizes;
  Int  cxt_hash_resizes;
  Int  fn_array_resizes;
  Int  call_stack_resizes;
  Int  fn_stack_resizes;
  Int  cfg_hash_resizes;

  Int  full_debug_BBs;
  Int  file_line_debug_BBs;
  Int  fn_name_debug_BBs;
  Int  no_debug_BBs;
  Int  bbcc_lru_misses;
  Int  cxt_lru_misses;
  Int  bbcc_clones;
};


/*------------------------------------------------------------*/
/*--- Structure declarations                               ---*/
/*------------------------------------------------------------*/

typedef struct _Context				Context;
typedef struct _CC					CC;
typedef struct _BB					BB;
typedef struct _BBCC					BBCC;
typedef struct _fCC					fCC;
typedef struct _fn_node				fn_node;
typedef struct _file_node			file_node;
typedef struct _obj_node				obj_node;
typedef struct _fn_config			fn_config;
typedef struct _call_entry			call_entry;
typedef struct _thread_info			thread_info;
typedef struct _BitSet				BitSet;
typedef struct _CFG					CFG;
typedef struct _CfgNode				CfgNode;
typedef struct _CfgInstrRef			CfgInstrRef;
typedef struct _CfgPathCache 		CfgPathCache;
typedef struct _FunctionDesc			FunctionDesc;
typedef struct _SmartHash			SmartHash;
typedef struct _SmartSeek			SmartSeek;

/* The types of control flow changes that can happen between
 * execution of two BBs in a thread.
 */
typedef enum {
  jk_None = 0,   /* no explicit change by a guest instruction */
  jk_Jump,       /* regular jump */
  jk_Call,
  jk_Return,
} LpgJumpKind;

typedef struct _InstrDesc InstrDesc;
struct _InstrDesc {
	HChar* name;
	Int lineno;
};

typedef struct _UniqueInstr UniqueInstr;
struct _UniqueInstr {
	Addr addr;
	Int size;
	HChar* name;
	InstrDesc* desc;
};

#define PATH_CACHE_SIZE 2
struct _CfgPathCache {
	struct {
		Addr from;
		Addr size;
		CfgInstrRef* to;
	} block[PATH_CACHE_SIZE];
	struct {
		Addr addr;
		Bool indirect;
	} phantom[PATH_CACHE_SIZE];
	struct {
		CFG* cfg;
		Bool indirect;
	} call;
	Bool exit;
};

struct _CfgInstrRef {
	UniqueInstr* instr;	// The instruction itself.
	CfgNode* node;		// Reference to the CFG node.

	CfgInstrRef* next;	// Next instruction in block. Nil if last.

#ifdef LPG_ENABLE_PATH_CACHE
	CfgPathCache* cache;	// Follow path cache.
#endif
};


/* 
 * Info for one instruction of a basic block.
 */
typedef struct _InstrInfo InstrInfo;
struct _InstrInfo {
  UInt instr_offset;
  UInt instr_size;
};

typedef struct _InstrGroupInfo InstrGroupInfo;
struct _InstrGroupInfo {
	Addr group_addr;   // address of the first instruction of the block.
	UInt group_size;   // size of the block (until the last instruction).
	UInt instr_count;  // the number of instructions in the block.

	struct {
		UInt first_instr; // first instruction index from the BB.
		UInt last_instr;  // last instruction index from the BB.
	} bb_info;
};

/*
 * Info for a side exit in a BB
 */
typedef struct _CJmpInfo CJmpInfo;
struct _CJmpInfo {
	UInt instr;          /* instruction index for BB.instr array */
	UInt group;          /* group index for BB.groups array */
	LpgJumpKind jmpkind; /* jump kind when leaving BB at this side exit */
	Addr dst;            /* Destination addr (nil if call or ret) */
	Bool indirect;       /* Mark if it is an indirect jump */
};

/**
 * An instrumented basic block (BB).
 *
 * BBs are put into a resizable hash to allow for fast detection if a
 * BB is to be retranslated but cost info is already available.
 * The key for a BB is a (object, offset) tupel making it independent
 * from possibly multiple mappings of the same ELF object.
 *
 * At the beginning of each instrumented BB,
 * a call to setup_bbcc(), specifying a pointer to the
 * according BB structure, is added.
 *
 * As cost of a BB has to be distinguished depending on the context,
 * multiple cost centers for one BB (struct BBCC) exist and the according
 * BBCC is set by setup_bbcc.
 */
struct _BB {
  obj_node*  obj;         /* ELF object of BB */
  PtrdiffT   offset;      /* offset of BB in ELF object file */
  BB*        next;       /* chaining for a hash entry */

  VgSectKind sect_kind;  /* section of this BB, e.g. PLT */
  UInt       instr_count;
  
  /* filled by LPG_(get_fn_node) if debug info is available */
  fn_node*   fn;          /* debug info for this BB */
  UInt       line;
  Bool       is_entry;    /* True if this BB is a function entry */
        
  BBCC*      bbcc_list;  /* BBCCs for same BB (see next_bbcc in BBCC) */
  BBCC*      last_bbcc;  /* Temporary: Cached for faster access (LRU) */

  /* filled by LPG_(instrument) if not seen before */
  UInt       cjmp_count;  /* number of side exits */
  CJmpInfo*  jmp;         /* array of info for condition jumps,
			   * allocated directly after this struct */
  Bool       cjmp_inverted; /* is last side exit actually fall through? */

  InstrGroupInfo* groups; /* array of instruction groups. */
  UInt groups_count;      /* number of groups */

  UInt       instr_len;
  InstrInfo  instr[0];   /* info on instruction sizes and costs */
};



/**
 * Function context
 *
 * Basic blocks are always executed in the scope of a context.
 * A function context is a list of function nodes representing
 * the call chain to the current context: I.e. fn[0] is the
 * function we are currently in, fn[1] has called fn[0], and so on.
 * Recursion levels are used for fn[0].
 *
 * To get a unique number for a full execution context, use
 *  rec_index = min(<fn->rec_separation>,<active>) - 1;
 *  unique_no = <number> + rec_index
 *
 * For each Context, recursion index and BB, there can be a BBCC.
 */
struct _Context {
    UInt size;        // number of function dependencies
    UInt base_number; // for context compression & dump array
    Context* next;    // entry chaining for hash
    UWord hash;       // for faster lookup...
    fn_node* fn[0];
};

/*
 * Basic Block Cost Center
 *
 * On demand, multiple BBCCs will be created for the same BB
 * dependent on command line options and:
 * - current function (it's possible that a BB is executed in the
 *   context of different functions, e.g. in manual assembler/PLT)
 * - current thread ID
 * - position where current function is called from
 * - recursion level of current function
 *
 * The cost centres for the instructions of a basic block are
 * stored in a contiguous array.
 * They are distinguishable by their tag field.
 */
struct _BBCC {
    BB*      bb;           /* BB for this cost center */

    Context* cxt;          /* execution context of this BBCC */
    ThreadId tid;          /* only for assertion check purpose */

    BBCC*    next_bbcc;    /* Chain of BBCCs for same BB */
    BBCC*    lru_next_bbcc; /* BBCC executed next the last time */
    
    BBCC*    next;         /* entry chain in hash */
};


/* the <number> of fn_node, file_node and obj_node are for compressed dumping
 * and a index into the dump boolean table and fn_info_table
 */

struct _fn_node {
  HChar*     name;
  UInt       number;
  Context*   last_cxt; /* LRU info */
  Context*   pure_cxt; /* the context with only the function itself */
  file_node* file;     /* reverse mapping for 2nd hash */
  fn_node* next;

  Bool is_malloc :1;
  Bool is_realloc :1;
  Bool is_free :1;

#if LPG_ENABLE_DEBUG
  Int  verbosity; /* Stores old verbosity level while in function */
#endif
};

/* Quite arbitrary fixed hash sizes */

#define   N_OBJ_ENTRIES         47
#define  N_FILE_ENTRIES         53
#define    N_FN_ENTRIES         87

struct _file_node {
   HChar*     name;
   fn_node*   fns[N_FN_ENTRIES];
   UInt       number;
   obj_node*  obj;
   file_node* next;
};

/* If an object is dlopened multiple times, we hope that <name> is unique;
 * <start> and <offset> can change with each dlopen, and <start> is
 * zero when object is unmapped (possible at dump time).
 */
struct _obj_node {
   const HChar* name;
   UInt       last_slash_pos;

   Addr       start;  /* Start address of text segment mapping */
   SizeT      size;   /* Length of mapping */
   PtrdiffT   offset; /* Offset between symbol address and file offset */

   file_node* files[N_FILE_ENTRIES];
   UInt       number;
   obj_node*  next;
};

/* an entry in the callstack */
struct _call_entry {
    Addr sp;            /* stack pointer directly after call */
    Addr ret_addr;      /* address to which to return to
			 * is 0 on a simulated call */
    Context* cxt;       /* context before call */
    Int fn_sp;          /* function stack index before call */

    CFG* cfg;
    CfgInstrRef* dangling;
};

/*
 * Execution state of main thread or a running signal handler in
 * a thread while interrupted by another signal handler.
 * As there's no scheduling among running signal handlers of one thread,
 * we only need a subset of a full thread state:
 * - event counter
 * - collect state
 * - last BB, last jump kind
 * - callstack pointer for sanity checking and correct unwinding
 *   after exit
 */
typedef struct _exec_state exec_state;
struct _exec_state {
  /* the signum of the handler, 0 for main thread context
   */
  Int sig;
  
  /* the old call stack pointer at entering the signal handler */
  Int orig_sp;
  
  Context* cxt;
  
  /* number of conditional jumps passed in last BB */
  Int   jmps_passed;
  BBCC* bbcc;      /* last BB executed */

  Int call_stack_bottom; /* Index into fn_stack */

  CFG* cfg;
  CfgInstrRef* dangling;
};

enum CfgNodeType {
	CFG_ENTRY,
	CFG_BLOCK,
	CFG_PHANTOM,
	CFG_EXIT,
	CFG_HALT
};

typedef struct _SmartValue SmartValue;
struct _SmartValue {
	Int index;
	void* value;
	SmartValue* next;
};

/* Global state structures */
typedef struct _bb_hash bb_hash;
struct _bb_hash {
  UInt size, entries;
  BB** table;
};

typedef struct _cxt_hash cxt_hash;
struct _cxt_hash {
  UInt size, entries;
  Context** table;
};  

/* Thread specific state structures, i.e. parts of a thread state.
 * There are variables for the current state of each part,
 * on which a thread state is copied at thread switch.
 */
typedef struct _bbcc_hash bbcc_hash;
struct _bbcc_hash {
  UInt size, entries;
  BBCC** table;
};

typedef struct _fn_array fn_array;
struct _fn_array {
  UInt size;
  UInt* array;
};

typedef struct _call_stack call_stack;
struct _call_stack {
  UInt size;
  Int sp;
  call_entry* entry;
};

typedef struct _fn_stack fn_stack;
struct _fn_stack {
  UInt size;
  fn_node **bottom, **top;
};

typedef struct _cfg_hash cfg_hash;
struct _cfg_hash {
  UInt size, entries;
  CFG** table;
};

/* The maximum number of simultaneous running signal handlers per thread.
 * This is the number of execution states storable in a thread.
 */
#define MAX_SIGHANDLERS 10

typedef struct _exec_stack exec_stack;
struct _exec_stack {
  Int sp; /* > 0 if a handler is running */
  exec_state* entry[MAX_SIGHANDLERS];
};

/* Thread State 
 *
 * This structure stores thread specific info while a thread is *not*
 * running. See function switch_thread() for save/restore on thread switch.
 *
 * If --separate-threads=no, BBCCs and JCCs can be shared by all threads, i.e.
 * only structures of thread 1 are used.
 * This involves variables fn_info_table, bbcc_table and jcc_table.
 */
struct _thread_info {
  /* state */
  fn_stack fns;       /* function stack */
  call_stack calls;   /* context call arc stack */
  exec_stack states;  /* execution states interrupted by signals */

  /* thread specific data structure containers */
  fn_array fn_active;
  bbcc_hash bbccs;
};

/*------------------------------------------------------------*/
/*--- Functions                                            ---*/
/*------------------------------------------------------------*/

/* from bb.c */
void LPG_(init_bb_hash)(void);
bb_hash* LPG_(get_bb_hash)(void);
BB*  LPG_(get_bb)(Addr addr, IRSB* bb_in, Bool *seen_before);
void LPG_(delete_bb)(Addr addr);

static __inline__ Addr bb_addr(BB* bb)
 { return bb->offset + bb->obj->offset; }
static __inline__ Addr bb_jmpaddr(BB* bb)
 { UInt off = (bb->instr_count > 0) ? bb->instr[bb->instr_count-1].instr_offset : 0;
   return off + bb->offset + bb->obj->offset; }

/* from bbcc.c */
void LPG_(init_bbcc_hash)(bbcc_hash* bbccs);
void LPG_(copy_current_bbcc_hash)(bbcc_hash* dst);
bbcc_hash* LPG_(get_current_bbcc_hash)(void);
void LPG_(set_current_bbcc_hash)(bbcc_hash*);
void LPG_(forall_bbccs)(void (*func)(BBCC*));
BBCC* LPG_(clone_bbcc)(BBCC* orig, Context* cxt);
BBCC* LPG_(get_bbcc)(BB* bb);
void LPG_(setup_bbcc)(BB* bb) VG_REGPARM(1);

/* from bitset.c */
BitSet* LPG_(new_bitset)(Int size);
BitSet* LPG_(new_fixed_bitset)(Int size);
void LPG_(delete_bitset)(BitSet* bs);
void LPG_(bitset_grow)(BitSet* bs, Int new_size);
void LPG_(bitset_copy)(BitSet* dst, BitSet* src);
BitSet* LPG_(bitset_clone)(BitSet* bs);
Int LPG_(bitset_size)(BitSet* bs);
Int LPG_(bitset_cardinality)(BitSet* bs);
Bool LPG_(bitset_cmp)(BitSet* bs1, BitSet* bs2);
Bool LPG_(bitset_is_empty)(BitSet* bs);
Bool LPG_(bitset_is_empty_pos)(BitSet* bs, Int pos);
Bool LPG_(bitset_is_empty_range)(BitSet* bs, Int from, Int to);
void LPG_(bitset_clear)(BitSet* bs);
void LPG_(bitset_clear_pos)(BitSet* bs, Int pos);
void LPG_(bitset_clear_range)(BitSet* bs, Int from, Int to);
Bool LPG_(bitset_get_pos)(BitSet* bs, Int pos);
BitSet* LPG_(bitset_get_range)(BitSet* bs, Int from, Int to);
void LPG_(bitset_set_pos)(BitSet* bs, Int pos);
void LPG_(bitset_set_pos_value)(BitSet* bs, Int pos, Bool value);
void LPG_(bitset_set_range)(BitSet* bs, Int from, Int to);
void LPG_(bitset_set_range_value)(BitSet* bs, Int from, Int to, Bool value);
void LPG_(bitset_flip)(BitSet* bs);
void LPG_(bitset_flip_pos)(BitSet* bs, Int pos);
void LPG_(bitset_flip_range)(BitSet* bs, Int from, Int to);
void LPG_(bitset_not)(BitSet* bs);
void LPG_(bitset_and)(BitSet* dst, BitSet* src);
void LPG_(bitset_or)(BitSet* dst, BitSet* src);
void LPG_(bitset_xor)(BitSet* dst, BitSet* src);
void LPG_(bitset_and_not)(BitSet* dst, BitSet* src);
void LPG_(bitset_forall_set)(BitSet* bs, Bool (*func)(Int, void*), void* arg);
void LPG_(bitset_print)(BitSet* bs);
SmartSeek* LPG_(bitset_seek)(BitSet* bs);
void LPG_(bitset_delete_seek)(SmartSeek* ss);
void LPG_(bitset_rewind)(SmartSeek* ss);
Int LPG_(bitset_get_index)(SmartSeek* ss);
void LPG_(bitset_set_index)(SmartSeek* ss, Int index);
void LPG_(bitset_clear_value)(SmartSeek* ss);
Bool LPG_(bitset_get)(SmartSeek* ss);
void LPG_(bitset_set)(SmartSeek* ss);
void LPG_(bitset_set_value)(SmartSeek* ss, Bool value);
Bool LPG_(bitset_has_next)(SmartSeek* ss);
void LPG_(bitset_next)(SmartSeek* ss);

/* from cfg.c */
void LPG_(init_cfg_hash)(void);
CFG* LPG_(get_cfg)(Addr addr);
Addr LPG_(cfg_addr)(CFG* cfg);
FunctionDesc* LPG_(cfg_fdesc)(CFG* cfg);
void LPG_(cfg_build_fdesc)(CFG* cfg);
Bool LPG_(cfg_is_inside_main)(CFG* cfg);
void LPG_(cfg_set_inside_main)(CFG* cfg, Bool inside_main);
Bool LPG_(cfg_is_dirty)(CFG* cfg);
Bool LPG_(cfg_is_visited)(CFG* cfg);
void LPG_(cfg_set_visited)(CFG* cfg, Bool visited);
Bool LPG_(cfg_is_complete)(CFG* cfg);
CfgNode* LPG_(cfg_entry_node)(CFG* cfg);
CfgNode* LPG_(cfg_exit_node)(CFG* cfg);
CfgNode* LPG_(cfg_halt_node)(CFG* cfg);
SmartList* LPG_(cfg_nodes)(CFG* cfg);
Bool LPG_(cfg_cmp)(CFG* cfg1, CFG* cfg2);
Int LPG_(cfgnode_id)(CfgNode* node);
enum CfgNodeType LPG_(cfgnode_type)(CfgNode* node);
const HChar* LPG_(cfgnode_type2str)(enum CfgNodeType type, Bool lowercase);
Addr LPG_(cfgnode_addr)(CfgNode* node);
Addr LPG_(cfgnode_size)(CfgNode* node);
SmartList* LPG_(cfgnode_successors)(CfgNode* node);
SmartList* LPG_(cfgnode_predecessors)(CfgNode* node);
SmartList* LPG_(cfgnode_dominators)(CfgNode* node);
void LPG_(cfgnode_set_dominators)(CfgNode* node, SmartList* dominators);
CfgNode* LPG_(cfgnode_immediate_dominator)(CfgNode* node);
void LPG_(cfgnode_set_immediate_dominator)(CfgNode* node, CfgNode* idom);
Bool LPG_(cfgnode_is_visited)(CfgNode* node);
void LPG_(cfgnode_set_visited)(CfgNode* node, Bool visited);
Bool LPG_(cfgnode_is_indirect)(CfgNode* node);
Bool LPG_(cfgnode_has_call_with_addr)(CfgNode* node, Addr addr);
Bool LPG_(cfgnode_has_successor_with_addr)(CfgNode* node, Addr addr, Bool* virtual);
Bool LPG_(cfgnodes_cmp)(CfgNode* node1, CfgNode* node2);
void LPG_(cfgnode_set_block)(CFG* cfg, CfgInstrRef** last, BB* bb, Int group_offset);
void LPG_(cfgnode_set_phantom)(CFG* cfg, CfgInstrRef* last, Addr to,
		LpgJumpKind jmpkind, Bool indirect);
void LPG_(cfgnode_set_call)(CFG* cfg, CfgInstrRef* last, CFG* call, Bool indirect);
void LPG_(cfgnode_set_exit)(CFG* cfg, CfgInstrRef** last);
void LPG_(cfgnode_set_halt)(CFG* cfg, CfgInstrRef** last);
void LPG_(clean_visited_cfgnodes)(CFG* cfg);
void LPG_(check_cfg)(CFG* cfg);
void LPG_(fprint_cfg)(VgFile* out, CFG* cfg);
void LPG_(fprint_detailed_cfg)(VgFile* out, CFG* cfg);
void LPG_(write_cfgs)(VgFile* out_fp);
void LPG_(read_cfgs)(Int fd);
void LPG_(dump_cfg)(CFG* cfg);
void LPG_(delete_cfg)(CFG* cfg);
void LPG_(forall_cfg)(void (*func)(CFG*), Bool all);
void LPG_(clear_visited)(CFG* cfg);
Bool LPG_(is_instr_leader)(UniqueInstr* instr);

/* from clo.c */
void LPG_(set_clo_defaults)(void);
Bool LPG_(process_cmd_line_option)(const HChar*);
void LPG_(print_usage)(void);
void LPG_(print_debug_usage)(void);

/* from context.c */
void LPG_(init_fn_stack)(fn_stack*);
void LPG_(copy_current_fn_stack)(fn_stack*);
void LPG_(set_current_fn_stack)(fn_stack*);

void LPG_(init_cxt_table)(void);
Context* LPG_(get_cxt)(fn_node** fn);
void LPG_(push_cxt)(fn_node* fn);

/* from fdesc.c */
FunctionDesc* LPG_(new_fdesc)(Addr addr, Bool entry);
void LPG_(delete_fdesc)(FunctionDesc* fdesc);
HChar* LPG_(fdesc_object_name)(FunctionDesc* fdesc);
HChar* LPG_(fdesc_function_name)(FunctionDesc* fdesc);
UInt LPG_(fdesc_function_line)(FunctionDesc* fdesc);
void LPG_(print_fdesc)(FunctionDesc* fdesc);
void LPG_(fprint_fdesc)(VgFile* fp, FunctionDesc* fdesc);
HChar* LPG_(fdesc2str)(FunctionDesc* fdesc);
FunctionDesc* LPG_(str2fdesc)(const HChar* str);
Bool LPG_(is_main_function)(FunctionDesc* fdesc);
Bool LPG_(compare_functions_desc)(FunctionDesc* fdesc1, FunctionDesc* fdesc2);

/* from fn.c */
void LPG_(init_fn_array)(fn_array*);
void LPG_(copy_current_fn_array)(fn_array* dst);
fn_array* LPG_(get_current_fn_array)(void);
void LPG_(set_current_fn_array)(fn_array*);
UInt* LPG_(get_fn_entry)(Int n);

void      LPG_(init_obj_table)(void);
obj_node* LPG_(get_obj_node)(DebugInfo* si);
file_node* LPG_(get_file_node)(obj_node*, const HChar *dirname,
                               const HChar* filename);
fn_node*  LPG_(get_fn_node)(BB* bb);

/* from main.c */
Bool LPG_(get_debug_info)(Addr, const HChar **dirname,
                          const HChar **filename,
                          const HChar **fn_name, UInt*, DebugInfo**);
void LPG_(collectBlockInfo)(IRSB* bbIn, UInt*, UInt*, Bool*, UInt *);
void LPG_(fini)(Int exitcode);

/* from smarthash.c */
SmartHash* LPG_(new_smart_hash)(Int size);
SmartHash* LPG_(new_fixed_smart_hash)(Int size);
void LPG_(delete_smart_hash)(SmartHash* shash);
void LPG_(smart_hash_clear)(SmartHash* shash, void (*remove_value)(void*));
Int LPG_(smart_hash_count)(SmartHash* shash);
Int LPG_(smart_hash_size)(SmartHash* shash);
Bool LPG_(smart_hash_is_empty)(SmartHash* shash);
Float LPG_(smart_hash_growth_rate)(SmartHash* shash);
void LPG_(smart_hash_set_growth_rate)(SmartHash* shash, Float rate);
void* LPG_(smart_hash_get)(SmartHash* shash, HWord key, HWord (*hash_key)(void*));
void* LPG_(smart_hash_put)(SmartHash* shash, void* value, HWord (*hash_key)(void*));
void* LPG_(smart_hash_remove)(SmartHash* shash, HWord key, HWord (*hash_key)(void*));
Bool LPG_(smart_hash_contains)(SmartHash* shash, HWord key, HWord (*hash_key)(void*));
void LPG_(smart_hash_forall)(SmartHash* shash, Bool (*func)(void*, void*), void* arg);
void LPG_(smart_hash_merge)(SmartHash* dst, SmartHash* src, HWord (*hash_key)(void*));

/* from smartlist.c */
SmartList* LPG_(new_smart_list)(Int size);
SmartList* LPG_(new_fixed_smart_list)(Int size);
SmartList* LPG_(clone_smart_list)(SmartList* slist);
void LPG_(delete_smart_list)(SmartList* slist);
void LPG_(smart_list_clear)(SmartList* slist, void (*remove_element)(void*));
Int LPG_(smart_list_size)(SmartList* slist);
Int LPG_(smart_list_count)(SmartList* slist);
Bool LPG_(smart_list_is_empty)(SmartList* slist);
void* LPG_(smart_list_at)(SmartList* slist, Int index);
void* LPG_(smart_list_head)(SmartList* slist);
void* LPG_(smart_list_tail)(SmartList* slist);
void LPG_(smart_list_set)(SmartList* slist, Int index, void* value);
void LPG_(smart_list_del)(SmartList* slist, Int index, Bool remove_contents);
void LPG_(smart_list_add)(SmartList* slist, void* value);
void LPG_(smart_list_copy)(SmartList* dst, SmartList* src);
void LPG_(smart_list_forall)(SmartList* slist, Bool (*func)(void*, void*), void* arg);
Bool LPG_(smart_list_contains)(SmartList* slist, void* value, Bool (*cmp)(void*, void*));
Float LPG_(smart_list_growth_rate)(SmartList* slist);
void LPG_(smart_list_set_growth_rate)(SmartList* slist, Float rate);
SmartValue* LPG_(smart_list_find)(SmartList* slist, Bool (*cmp)(void*, void*), void* arg);
void LPG_(smart_list_delete_value)(SmartValue* sv);
SmartSeek* LPG_(smart_list_seek)(SmartList* slist);
void LPG_(smart_list_delete_seek)(SmartSeek* ss);
void LPG_(smart_list_rewind)(SmartSeek* ss);
Int LPG_(smart_list_get_index)(SmartSeek* ss);
void LPG_(smart_list_set_index)(SmartSeek* ss, Int index);
Bool LPG_(smart_list_has_next)(SmartSeek* ss);
void LPG_(smart_list_next)(SmartSeek* ss);
void* LPG_(smart_list_get_value)(SmartSeek* ss);
void LPG_(smart_list_set_value)(SmartSeek* ss, void* value);

/* from instrs.c */
void LPG_(init_instrs_pool)(void);
void LPG_(destroy_instrs_pool)(void);
UniqueInstr* LPG_(get_instr)(Addr addr, Int size);
UniqueInstr* LPG_(find_instr)(Addr addr);
Addr LPG_(instr_addr)(UniqueInstr* instr);
Int LPG_(instr_size)(UniqueInstr* instr);
const HChar* LPG_(instr_name)(UniqueInstr* instr);
InstrDesc* LPG_(instr_description)(UniqueInstr* instr);
Bool LPG_(instrs_cmp)(UniqueInstr* i1, UniqueInstr* i2);
void LPG_(print_instr)(UniqueInstr* instr, Bool complete);
void LPG_(fprint_instr)(VgFile* fp, UniqueInstr* instr, Bool complete);
void LPG_(print_instr_description)(InstrDesc* idesc);
void LPG_(fprint_instr_description)(VgFile* fp, InstrDesc* idesc);

/* from callstack.c */
void LPG_(init_call_stack)(call_stack*);
void LPG_(copy_current_call_stack)(call_stack* dst);
void LPG_(set_current_call_stack)(call_stack*);
call_entry* LPG_(get_call_entry)(Int n);
void LPG_(push_call_stack)(BBCC* from, UInt jmp, BBCC* to, Addr sp);
void LPG_(pop_call_stack)(Bool halt);
Int LPG_(unwind_call_stack)(Addr sp, Int);

/* from threads.c */
void LPG_(init_threads)(void);
thread_info** LPG_(get_threads)(void);
thread_info* LPG_(get_current_thread)(void);
void LPG_(switch_thread)(ThreadId tid);
void LPG_(forall_threads)(void (*func)(thread_info*));
void LPG_(run_thread)(ThreadId tid);

void LPG_(init_exec_state)(exec_state* es);
void LPG_(init_exec_stack)(exec_stack*);
void LPG_(copy_current_exec_stack)(exec_stack*);
void LPG_(set_current_exec_stack)(exec_stack*);
void LPG_(pre_signal)(ThreadId tid, Int sigNum, Bool alt_stack);
void LPG_(post_signal)(ThreadId tid, Int sigNum);
void LPG_(run_post_signal_on_call_stack_bottom)(void);

/*------------------------------------------------------------*/
/*--- Exported global variables                            ---*/
/*------------------------------------------------------------*/

extern CommandLineOptions LPG_(clo);
extern Statistics LPG_(stat);

/* Function active counter array, indexed by function number */
extern UInt* LPG_(fn_active_array);
 /* min of L1 and LL cache line sizes */
extern call_stack LPG_(current_call_stack);
extern fn_stack   LPG_(current_fn_stack);
extern exec_state LPG_(current_state);
extern ThreadId   LPG_(current_tid);

/*------------------------------------------------------------*/
/*--- Debug output                                         ---*/
/*------------------------------------------------------------*/

#if LPG_ENABLE_DEBUG

#define LPG_DEBUGIF(x) \
  if (UNLIKELY( (LPG_(clo).verbose >x) && \
                (LPG_(stat).bb_executions >= LPG_(clo).verbose_start)))

#define LPG_DEBUG(x,format,args...)   \
    LPG_DEBUGIF(x) {                  \
      LPG_(print_bbno)();	      \
      VG_(printf)(format,##args);     \
    }

#define LPG_ASSERT(cond)              \
    if (UNLIKELY(!(cond))) {          \
      LPG_(print_context)();          \
      LPG_(print_bbno)();	      \
      tl_assert(cond);                \
     }

#else
#define LPG_DEBUGIF(x) if (0)
#define LPG_DEBUG(x...) {}
#define LPG_ASSERT(cond) tl_assert(cond);
#endif

/* from debug.c */
void LPG_(print_bbno)(void);
void LPG_(print_context)(void);
void LPG_(print_bbcc)(int s, BBCC* bbcc);
void LPG_(print_bbcc_fn)(BBCC* bbcc);
void LPG_(print_execstate)(int s, exec_state* es);
void LPG_(print_bb)(int s, BB* bb);
void LPG_(print_cxt)(int s, Context* cxt);
void LPG_(print_stackentry)(int s, int sp);
void LPG_(print_addr)(Addr addr);
void LPG_(print_addr_ln)(Addr addr);

#if LPG_DEBUG_MEM
void* LPG_(malloc)(const HChar* cc, UWord s, const HChar* f);
void* LPG_(realloc)(const HChar* cc, void* p, UWord s, const HChar* f);
void LPG_(free)(void* p, const HChar* f);
HChar* LPG_(strdup)(const HChar* cc, const HChar* s, const HChar* f);

#define LPG_MALLOC(_cc,x)		LPG_(malloc)((_cc),x,__FUNCTION__)
#define LPG_FREE(p)				LPG_(free)(p,__FUNCTION__)
#define LPG_REALLOC(_cc,p,x)		LPG_(realloc)((_cc),p,x,__FUNCTION__)
#define LPG_STRDUP(_cc,s)		LPG_(strdup)((_cc),s,__FUNCTION__)
#else
#define LPG_MALLOC(_cc,x)		VG_(malloc)((_cc),x)
#define LPG_FREE(p)				VG_(free)(p)
#define LPG_REALLOC(_cc,p,x)		VG_(realloc)((_cc),p,x)
#define LPG_STRDUP(_cc,s)		VG_(strdup)((_cc),s)
#endif

#define LPG_UNUSED(arg)			(void)arg;
#define LPG_DATA_FREE(p,x)		\
	do { 						\
		VG_(memset)(p, 0x41, x);	\
		LPG_FREE(p); 			\
	} while (0)

#endif /* LPG_GLOBAL */
