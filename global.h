/*--------------------------------------------------------------------*/
/*--- CFGgrind                                                     ---*/
/*---                                                     global.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of CFGgrind, a dynamic control flow graph (CFG)
   reconstruction tool.

   Copyright (C) 2019, Andrei Rimsa (andrei@cefetmg.br)

   This tool is derived from and contains lot of code from Callgrind
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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef CGD_GLOBAL
#define CGD_GLOBAL

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


#define CGD_(str) VGAPPEND(vgCFGgrind_,str)

/*------------------------------------------------------------*/
/*--- Callgrind compile options                           --- */
/*------------------------------------------------------------*/

/* Enable debug output */
#define CGD_ENABLE_DEBUG	1
#define CGD_DEBUG_MEM	0

/* Syscall Timing in microseconds? 
 * (define to 0 if you get compile errors) */
#define CGD_MICROSYSTIME 0

// CFG node cache size. Use 0 to disable.
#define CFG_NODE_CACHE_SIZE 8

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
  Bool emulate_calls;       /* Emulate calls for some jumps */
  struct {
	  Bool all;             /* Dump all cfgs */
	  SmartList* addrs;     /* List of cfg's addresses to dump */
	  SmartList* fnames;    /* List of cfg's function names to dump */
	  const HChar* dir;     /* Directory where to dump the cfg's */
  } dump_cfgs;
  const HChar* instrs_map;   /* Instructions map input file */
  const HChar* mem_mappings; /* Runtime memory mappings output file */

#if CGD_ENABLE_DEBUG
  Int   verbose;
  ULong verbose_start;
#endif
};

/*------------------------------------------------------------*/
/*--- Statistics                                           ---*/
/*------------------------------------------------------------*/

typedef struct _Statistics Statistics;
struct _Statistics {
  ULong bb_executions;
  Int  bb_retranslations;  

  Int  distinct_objs;
  Int  distinct_files;
  Int  distinct_fns;
  Int  distinct_bbs;
  Int  distinct_instrs;
  Int  distinct_groups;
  Int  distinct_cfgs;
  Int  distinct_cfg_nodes;

  Int  bb_hash_resizes;
  Int  call_stack_resizes;
  Int  cfg_hash_resizes;

  Int  full_debug_BBs;
  Int  file_line_debug_BBs;
  Int  fn_name_debug_BBs;
  Int  no_debug_BBs;
};


/*------------------------------------------------------------*/
/*--- Structure declarations                               ---*/
/*------------------------------------------------------------*/

typedef struct _BB					BB;
typedef struct _fn_node				fn_node;
typedef struct _file_node			file_node;
typedef struct _obj_node				obj_node;
typedef struct _call_entry			call_entry;
typedef struct _thread_info			thread_info;
typedef struct _CFG					CFG;
typedef struct _CfgInstrRef			CfgInstrRef;
typedef struct _CfgNode				CfgNode;
typedef struct _CfgEdge				CfgEdge;
typedef struct _CfgBlock				CfgBlock;
typedef struct _FunctionDesc			FunctionDesc;
typedef struct _SmartHash			SmartHash;
typedef struct _SmartSeek			SmartSeek;

/* The types of control flow changes that can happen between
 * execution of two BBs in a thread.
 */
typedef enum {
  bjk_None = 0,   /* no explicit change by a guest instruction */
  bjk_Jump,       /* regular jump */
  bjk_Call,
  bjk_Return,
} BBJumpKind;

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
	BBJumpKind jmpkind; /* jump kind when leaving BB at this side exit */
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
 * a call to setup_bb(), specifying a pointer to the
 * according BB structure, is added.
 */
struct _BB {
  obj_node*  obj;         /* ELF object of BB */
  PtrdiffT   offset;      /* offset of BB in ELF object file */
  BB*        next;       /* chaining for a hash entry */

  VgSectKind sect_kind;  /* section of this BB, e.g. PLT */
  UInt       instr_count;
  
  /* filled by CGD_(get_fn_node) if debug info is available */
  fn_node*   fn;          /* debug info for this BB */
  UInt       line;
  Bool       is_entry;    /* True if this BB is a function entry */

  /* filled by CGD_(instrument) if not seen before */
  UInt       cjmp_count;  /* number of side exits */
  CJmpInfo*  jmp;         /* array of info for condition jumps,
			   * allocated directly after this struct */
  Bool       cjmp_inverted; /* is last side exit actually fall through? */

  InstrGroupInfo* groups; /* array of instruction groups. */
  UInt groups_count;      /* number of groups */

  UInt       instr_len;
  InstrInfo  instr[0];   /* info on instruction sizes and costs */
};

/* the <number> of fn_node, file_node and obj_node are for compressed dumping
 * and a index into the dump boolean table and fn_info_table
 */
struct _fn_node {
  HChar*     name;
  UInt       number;
  Bool		visited;
  file_node* file;     /* reverse mapping for 2nd hash */
  fn_node* next;

  Bool is_malloc :1;
  Bool is_realloc :1;
  Bool is_free :1;

#if CGD_ENABLE_DEBUG
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
   HChar* name;
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

    CFG* cfg;
    CfgNode* working;
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
  
  /* number of conditional jumps passed in last BB */
  Int   jmps_passed;
  BB*   bb;      /* last BB executed */

  Int call_stack_bottom; /* Index into fn_stack */

  CFG* cfg;
  CfgNode* working;
};

enum CfgNodeType {
	CFG_ENTRY,
	CFG_BLOCK,
	CFG_PHANTOM,
	CFG_EXIT,
	CFG_HALT
};

struct _CFG {
	Addr addr;				// CFG address
	FunctionDesc* fdesc;		// debugging info for this CFG

	Bool dirty;				// true if new nodes are added during analysis
	Bool visited;			// used to use in search algorithms

	CfgNode* entry;			// cfg entry node
	CfgNode* exit;			// cfg exit node (if exists).
	CfgNode* halt;			// cfg halt node (if exists).
	SmartList* nodes;		// SmartList<CfgNode*>
	SmartList* edges;		// SmartList<Edge*>

	struct {
		SmartHash* refs;		// SmartHash<CfgInstrRef*>, index by instruction address
	} cache;

	struct {
		ULong execs;
		Int blocks;
		Int phantoms;
		Int indirects;
	} stats;

	CFG* chain;				// entry chain in hash
};

#if CFG_NODE_CACHE_SIZE > 0
#define CFG_NODE_CACHE_INDEX(addr) (addr % CFG_NODE_CACHE_SIZE)

typedef struct _CfgNodeBlockCache		CfgNodeBlockCache;
struct _CfgNodeBlockCache {
	Addr addr;
	UInt size;
	ULong count;
	CfgNode* working;
};

typedef struct _CfgNodePhantomCache		CfgNodePhantomCache;
struct _CfgNodePhantomCache {
	Addr addr;
	Bool indirect;
};

typedef struct _CfgNodeCallCache		CfgNodeCallCache;
struct _CfgNodeCallCache {
	Addr addr;
	Bool indirect;
};

typedef struct _CfgNodeExitCache		CfgNodeExitCache;
struct _CfgNodeExitCache {
	Bool enabled;
	ULong count;
};
#endif

struct _CfgNode {
	Int id;
	enum CfgNodeType type;

	union {
		CfgInstrRef* phantom;	/* Phantom instruction */
		CfgBlock* block;			/* Block node's block */
	} data;

	struct {
		SmartList* successors;   /* SmartList<CfgNode*> */
		SmartList* predecessors; /* SmartList<CfgNode*> */
	} info;

#if CFG_NODE_CACHE_SIZE > 0
	struct  {
		CfgNodeBlockCache* block;     // CfgNodeBlockCache block[CFG_NODE_CACHE_SIZE];
		CfgNodePhantomCache* phantom; // CfgNodePhantomCache phantom[CFG_NODE_CACHE_SIZE];
		CfgNodeCallCache* call;       // CfgNodeCallCache call[CFG_NODE_CACHE_SIZE];
		CfgNodeExitCache exit;
	} cache;
#endif

	Bool visited;				// mark of visited node
};

struct _CfgEdge {
	CfgNode* src;
	CfgNode* dst;
	ULong count;
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

typedef struct _call_stack call_stack;
struct _call_stack {
  UInt size;
  Int sp;
  call_entry* entry;
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
 */
struct _thread_info {
  /* state */
  call_stack calls;   /* context call arc stack */
  exec_stack states;  /* execution states interrupted by signals */
};

/*------------------------------------------------------------*/
/*--- Functions                                            ---*/
/*------------------------------------------------------------*/

/* from bb.c */
void CGD_(init_bb_hash)(void);
void CGD_(destroy_bb_hash)(void);
bb_hash* CGD_(get_bb_hash)(void);
BB*  CGD_(get_bb)(Addr addr, IRSB* bb_in, Bool *seen_before);
void CGD_(delete_bb)(Addr addr);
void CGD_(setup_bb)(BB* bb) VG_REGPARM(1);

static __inline__ Addr bb_addr(BB* bb)
 { return bb->offset + bb->obj->offset; }
static __inline__ Addr bb_jmpaddr(BB* bb)
 { UInt off = (bb->instr_count > 0) ? bb->instr[bb->instr_count-1].instr_offset : 0;
   return off + bb->offset + bb->obj->offset; }

/* from cfg.c */
void CGD_(init_cfg_hash)(void);
void CGD_(destroy_cfg_hash)(void);
CFG* CGD_(get_cfg)(Addr addr);
Addr CGD_(cfg_addr)(CFG* cfg);
FunctionDesc* CGD_(cfg_fdesc)(CFG* cfg);
void CGD_(cfg_build_fdesc)(CFG* cfg);
Bool CGD_(cfg_is_dirty)(CFG* cfg);
Bool CGD_(cfg_is_visited)(CFG* cfg);
void CGD_(cfg_set_visited)(CFG* cfg, Bool visited);
Bool CGD_(cfg_is_complete)(CFG* cfg);
CfgNode* CGD_(cfg_entry_node)(CFG* cfg);
CfgNode* CGD_(cfg_exit_node)(CFG* cfg);
CfgNode* CGD_(cfg_halt_node)(CFG* cfg);
SmartList* CGD_(cfg_nodes)(CFG* cfg);
Bool CGD_(cfg_cmp)(CFG* cfg1, CFG* cfg2);
Int CGD_(cfgnode_id)(CfgNode* node);
enum CfgNodeType CGD_(cfgnode_type)(CfgNode* node);
const HChar* CGD_(cfgnode_type2str)(enum CfgNodeType type, Bool lowercase);
Addr CGD_(cfgnode_addr)(CfgNode* node);
Int CGD_(cfgnode_size)(CfgNode* node);
SmartList* CGD_(cfgnode_successors)(CfgNode* node);
SmartList* CGD_(cfgnode_predecessors)(CfgNode* node);
Bool CGD_(cfgnode_is_visited)(CfgNode* node);
void CGD_(cfgnode_set_visited)(CfgNode* node, Bool visited);
Bool CGD_(cfgnode_is_indirect)(CfgNode* node);
Bool CGD_(cfgnode_has_call_with_addr)(CfgNode* node, Addr addr);
Bool CGD_(cfgnode_has_successor_with_addr)(CfgNode* node, Addr addr);
void CGD_(cfgnode_remove_successor_with_addr)(CFG* cfg, CfgNode* node, Addr addr);
Bool CGD_(cfgnodes_cmp)(CfgNode* node1, CfgNode* node2);
CfgNode* CGD_(cfgnode_set_block)(CFG* cfg, CfgNode* working, BB* bb, Int group_offset);
void CGD_(cfgnode_set_phantom)(CFG* cfg, CfgNode* working, Addr to,
		BBJumpKind jmpkind, Bool indirect);
void CGD_(cfgnode_set_call)(CFG* cfg, CfgNode* working, CFG* call, Bool indirect);
CfgNode* CGD_(cfgnode_set_exit)(CFG* cfg, CfgNode* working);
CfgNode* CGD_(cfgnode_set_halt)(CFG* cfg, CfgNode* working);
void CGD_(clean_visited_cfgnodes)(CFG* cfg);
void CGD_(check_cfg)(CFG* cfg);
void CGD_(fprint_cfg)(VgFile* out, CFG* cfg);
void CGD_(fprint_detailed_cfg)(VgFile* out, CFG* cfg);
void CGD_(write_cfgs)(const HChar* filename);
void CGD_(read_cfgs)(Int fd);
void CGD_(dump_cfg)(CFG* cfg);
void CGD_(forall_cfg)(void (*func)(CFG*));
void CGD_(clear_visited)(CFG* cfg);
#if CFG_NODE_CACHE_SIZE > 0
void CGD_(cfgnode_flush_count)(CFG* cfg, CfgNode* working, CfgNodeBlockCache* cache);
void CGD_(cfg_flush_all_counts)(CFG* cfg);
#endif

/* from clo.c */
void CGD_(set_clo_defaults)(void);
Bool CGD_(process_cmd_line_option)(const HChar*);
void CGD_(print_usage)(void);
void CGD_(print_debug_usage)(void);

/* from fdesc.c */
FunctionDesc* CGD_(new_fdesc)(Addr addr, Bool entry);
void CGD_(delete_fdesc)(FunctionDesc* fdesc);
HChar* CGD_(fdesc_object_name)(FunctionDesc* fdesc);
HChar* CGD_(fdesc_function_name)(FunctionDesc* fdesc);
UInt CGD_(fdesc_function_line)(FunctionDesc* fdesc);
void CGD_(print_fdesc)(FunctionDesc* fdesc);
void CGD_(fprint_fdesc)(VgFile* fp, FunctionDesc* fdesc);
HChar* CGD_(fdesc2str)(FunctionDesc* fdesc);
FunctionDesc* CGD_(str2fdesc)(const HChar* str);
Bool CGD_(is_main_function)(FunctionDesc* fdesc);
Bool CGD_(compare_functions_desc)(FunctionDesc* fdesc1, FunctionDesc* fdesc2);

/* from fn.c */
void CGD_(init_obj_table)(void);
void CGD_(destroy_obj_table)(void);
obj_node* CGD_(get_obj_node)(DebugInfo* si);
file_node* CGD_(get_file_node)(obj_node*, const HChar *dirname,
                               const HChar* filename);
fn_node*  CGD_(get_fn_node)(BB* bb);

/* from main.c */
Bool CGD_(get_debug_info)(Addr, const HChar **dirname,
                          const HChar **filename,
                          const HChar **fn_name, UInt*, DebugInfo**);
void CGD_(collectBlockInfo)(IRSB* bbIn, UInt*, UInt*, Bool*, UInt *);
void CGD_(fini)(Int exitcode);

/* from smarthash.c */
SmartHash* CGD_(new_smart_hash)(Int size);
SmartHash* CGD_(new_fixed_smart_hash)(Int size);
void CGD_(delete_smart_hash)(SmartHash* shash);
void CGD_(smart_hash_clear)(SmartHash* shash, void (*remove_value)(void*));
Int CGD_(smart_hash_count)(SmartHash* shash);
Int CGD_(smart_hash_size)(SmartHash* shash);
Bool CGD_(smart_hash_is_empty)(SmartHash* shash);
Float CGD_(smart_hash_growth_rate)(SmartHash* shash);
void CGD_(smart_hash_set_growth_rate)(SmartHash* shash, Float rate);
void* CGD_(smart_hash_get)(SmartHash* shash, HWord key, HWord (*hash_key)(void*));
void* CGD_(smart_hash_put)(SmartHash* shash, void* value, HWord (*hash_key)(void*));
void* CGD_(smart_hash_remove)(SmartHash* shash, HWord key, HWord (*hash_key)(void*));
Bool CGD_(smart_hash_contains)(SmartHash* shash, HWord key, HWord (*hash_key)(void*));
void CGD_(smart_hash_forall)(SmartHash* shash, Bool (*func)(void*, void*), void* arg);
void CGD_(smart_hash_merge)(SmartHash* dst, SmartHash* src, HWord (*hash_key)(void*));

/* from smartlist.c */
SmartList* CGD_(new_smart_list)(Int size);
SmartList* CGD_(new_fixed_smart_list)(Int size);
SmartList* CGD_(clone_smart_list)(SmartList* slist);
void CGD_(delete_smart_list)(SmartList* slist);
void CGD_(smart_list_clear)(SmartList* slist, void (*remove_element)(void*));
Int CGD_(smart_list_size)(SmartList* slist);
Int CGD_(smart_list_count)(SmartList* slist);
Bool CGD_(smart_list_is_empty)(SmartList* slist);
void* CGD_(smart_list_at)(SmartList* slist, Int index);
void* CGD_(smart_list_head)(SmartList* slist);
void* CGD_(smart_list_tail)(SmartList* slist);
void CGD_(smart_list_set)(SmartList* slist, Int index, void* value);
void CGD_(smart_list_del)(SmartList* slist, Int index, Bool remove_contents);
void CGD_(smart_list_add)(SmartList* slist, void* value);
void CGD_(smart_list_copy)(SmartList* dst, SmartList* src);
void CGD_(smart_list_forall)(SmartList* slist, Bool (*func)(void*, void*), void* arg);
Bool CGD_(smart_list_contains)(SmartList* slist, void* value, Bool (*cmp)(void*, void*));
Float CGD_(smart_list_growth_rate)(SmartList* slist);
void CGD_(smart_list_set_growth_rate)(SmartList* slist, Float rate);
SmartValue* CGD_(smart_list_find)(SmartList* slist, Bool (*cmp)(void*, void*), void* arg);
void CGD_(smart_list_delete_value)(SmartValue* sv);
SmartSeek* CGD_(smart_list_seek)(SmartList* slist);
void CGD_(smart_list_delete_seek)(SmartSeek* ss);
void CGD_(smart_list_rewind)(SmartSeek* ss);
Int CGD_(smart_list_get_index)(SmartSeek* ss);
void CGD_(smart_list_set_index)(SmartSeek* ss, Int index);
Bool CGD_(smart_list_has_next)(SmartSeek* ss);
void CGD_(smart_list_next)(SmartSeek* ss);
void* CGD_(smart_list_get_value)(SmartSeek* ss);
void CGD_(smart_list_set_value)(SmartSeek* ss, void* value);

/* from instrs.c */
void CGD_(init_instrs_pool)(void);
void CGD_(destroy_instrs_pool)(void);
UniqueInstr* CGD_(get_instr)(Addr addr, Int size);
UniqueInstr* CGD_(find_instr)(Addr addr);
Addr CGD_(instr_addr)(UniqueInstr* instr);
Int CGD_(instr_size)(UniqueInstr* instr);
const HChar* CGD_(instr_name)(UniqueInstr* instr);
InstrDesc* CGD_(instr_description)(UniqueInstr* instr);
Bool CGD_(instrs_cmp)(UniqueInstr* i1, UniqueInstr* i2);
void CGD_(print_instr)(UniqueInstr* instr, Bool complete);
void CGD_(fprint_instr)(VgFile* fp, UniqueInstr* instr, Bool complete);
void CGD_(print_instr_description)(InstrDesc* idesc);
void CGD_(fprint_instr_description)(VgFile* fp, InstrDesc* idesc);

/* from callstack.c */
void CGD_(init_call_stack)(call_stack* s);
void CGD_(destroy_call_stack)(call_stack* s);
void CGD_(copy_current_call_stack)(call_stack* dst);
void CGD_(set_current_call_stack)(call_stack* s);
call_entry* CGD_(get_call_entry)(Int n);
void CGD_(push_call_stack)(BB* from, UInt jmp, BB* to, Addr sp);
void CGD_(pop_call_stack)(Bool halt);
Int CGD_(unwind_call_stack)(Addr sp, Int);

/* from threads.c */
void CGD_(init_threads)(void);
void CGD_(destroy_threads)(void);
thread_info** CGD_(get_threads)(void);
thread_info* CGD_(get_current_thread)(void);
void CGD_(switch_thread)(ThreadId tid);
void CGD_(forall_threads)(void (*func)(thread_info*));
void CGD_(run_thread)(ThreadId tid);

void CGD_(init_exec_state)(exec_state* es);
void CGD_(init_exec_stack)(exec_stack* es);
void CGD_(destroy_exec_stack)(exec_stack* es);
void CGD_(copy_current_exec_stack)(exec_stack* dst);
void CGD_(set_current_exec_stack)(exec_stack* dst);
void CGD_(pre_signal)(ThreadId tid, Int sigNum, Bool alt_stack);
void CGD_(post_signal)(ThreadId tid, Int sigNum);
void CGD_(run_post_signal_on_call_stack_bottom)(void);

/*------------------------------------------------------------*/
/*--- Exported global variables                            ---*/
/*------------------------------------------------------------*/

extern CommandLineOptions CGD_(clo);
extern Statistics CGD_(stat);

/* Function active counter array, indexed by function number */
extern UInt* CGD_(fn_active_array);
 /* min of L1 and LL cache line sizes */
extern call_stack CGD_(current_call_stack);
extern exec_state CGD_(current_state);
extern ThreadId   CGD_(current_tid);

/*------------------------------------------------------------*/
/*--- Debug output                                         ---*/
/*------------------------------------------------------------*/

#if CGD_ENABLE_DEBUG

#define CGD_DEBUGIF(x) \
  if (UNLIKELY( (CGD_(clo).verbose >x) && \
                (CGD_(stat).bb_executions >= CGD_(clo).verbose_start)))

#define CGD_DEBUG(x,format,args...)   \
    CGD_DEBUGIF(x) {                  \
      CGD_(print_bbno)();	      \
      VG_(printf)(format,##args);     \
    }

#define CGD_ASSERT(cond)              \
    if (UNLIKELY(!(cond))) {          \
      CGD_(print_bbno)();	      \
      tl_assert(cond);                \
     }

#else
#define CGD_DEBUGIF(x) if (0)
#define CGD_DEBUG(x...) {}
#define CGD_ASSERT(cond) tl_assert(cond);
#endif

/* from debug.c */
void CGD_(print_bbno)(void);
void CGD_(print_execstate)(int s, exec_state* es);
void CGD_(print_bb)(int s, BB* bb);
void CGD_(print_stackentry)(int s, int sp);
void CGD_(print_addr)(Addr addr);
void CGD_(print_addr_ln)(Addr addr);

#if CGD_DEBUG_MEM
void* CGD_(malloc)(const HChar* cc, UWord s, const HChar* f);
void* CGD_(realloc)(const HChar* cc, void* p, UWord s, const HChar* f);
void CGD_(free)(void* p, const HChar* f);
HChar* CGD_(strdup)(const HChar* cc, const HChar* s, const HChar* f);

#define CGD_MALLOC(_cc,x)		CGD_(malloc)((_cc),x,__FUNCTION__)
#define CGD_FREE(p)				CGD_(free)(p,__FUNCTION__)
#define CGD_REALLOC(_cc,p,x)		CGD_(realloc)((_cc),p,x,__FUNCTION__)
#define CGD_STRDUP(_cc,s)		CGD_(strdup)((_cc),s,__FUNCTION__)
#else
#define CGD_MALLOC(_cc,x)		VG_(malloc)((_cc),x)
#define CGD_FREE(p)				VG_(free)(p)
#define CGD_REALLOC(_cc,p,x)		VG_(realloc)((_cc),p,x)
#define CGD_STRDUP(_cc,s)		VG_(strdup)((_cc),s)
#endif

#define CGD_UNUSED(arg)			(void)arg;
#define CGD_DATA_FREE(p,x)		\
	do { 						\
		VG_(memset)(p, 0x41, x);	\
		CGD_FREE(p); 			\
	} while (0)

#endif /* CGD_GLOBAL */
