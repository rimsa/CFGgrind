/*--------------------------------------------------------------------*/
/*--- CFGgrind                                                     ---*/
/*---                                                        cfg.c ---*/
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

struct _CfgInstrRef {
	UniqueInstr* instr;	// The instruction itself.
	CfgNode* node;		// Reference to the CFG node.

	CfgInstrRef* next;	// Next instruction in block. Nil if last.
};

struct _CfgCall {
	CFG* called;
#if ENABLE_PROFILING
	unsigned long long count;
#endif
};

struct _CfgSignalHandler {
	Int signum;
	CfgCall* handler;
};

struct _CfgBlock {
	Addr addr;
	Int size;

	struct {
		CfgInstrRef* leader;
		CfgInstrRef* tail;
		Int count;
	} instrs;

	// CfgBlock can have calls to somewhere.
	SmartList* calls;		// SmartList<CfgCall*>
	SmartList* sighandlers;	// SmartList<CfgSignalHandler*>

	Bool indirect;				/* has an indirect call or jump */
};

/* CFG hash, resizable */
cfg_hash cfgs;

VgFile *fp = 0;

struct {
	enum {
		TKN_BRACKET_OPEN,
		TKN_BRACKET_CLOSE,
		TKN_COLON,
		TKN_ARROW,
		TKN_CFG,
		TKN_NODE,
		TKN_EXIT,
		TKN_HALT,
		TKN_ADDR,
		TKN_NUMBER,
		TKN_BOOL,
		TKN_TEXT
	} type;

	union {
		Addr addr;
		ULong number;
		Bool bool;
	} data;

	HChar text[8192];
} token;

static __inline__
Addr ref_instr_addr(CfgInstrRef* ref) {
	CGD_ASSERT(ref != 0 && ref->instr != 0);
	return ref->instr->addr;
}

static __inline__
CfgInstrRef* new_instr_ref(UniqueInstr* instr) {
	CfgInstrRef* ref;

	CGD_ASSERT(instr != 0);

	ref = (CfgInstrRef*) CGD_MALLOC("cgd.cfg.nil.1", sizeof(CfgInstrRef));
	VG_(memset)(ref, 0, sizeof(CfgInstrRef));

	ref->instr = instr;

	return ref;
}

static __inline__
void delete_instr_ref(CfgInstrRef* ref) {
	CGD_ASSERT(ref != 0);
	CGD_DATA_FREE(ref, sizeof(CfgInstrRef));
}

static __inline__
UInt cfg_hash_idx(Addr addr, UInt size) {
	return addr % size;
}

/* double size of cfg table  */
static
void resize_cfg_table(void) {
    Int i, new_size, conflicts1 = 0;
    CFG **new_table, *curr, *next;
    UInt new_idx;

    new_size  = 2 * cfgs.size + 3;
    new_table = (CFG**) CGD_MALLOC("cgd.cfg.rct.1",
                                  (new_size * sizeof(CFG*)));
    VG_(memset)(new_table, 0, (new_size * sizeof(CFG*)));

    for (i = 0; i < cfgs.size; i++) {
		if (cfgs.table[i] == 0)
			continue;

		curr = cfgs.table[i];
		while (curr != 0) {
			next = curr->chain;

			new_idx = cfg_hash_idx(curr->addr, new_size);

			curr->chain = new_table[new_idx];
			new_table[new_idx] = curr;
			if (curr->chain)
				conflicts1++;

			curr = next;
		}
    }

    CGD_FREE(cfgs.table);

    CGD_DEBUG(0, "Resize CFG Hash: %u => %d (entries %u, conflicts %d)\n",
	     cfgs.size, new_size,
	     cfgs.entries, conflicts1);

    cfgs.size  = new_size;
    cfgs.table = new_table;
    CGD_(stat).cfg_hash_resizes++;
}

static
CFG* lookup_cfg(Addr addr) {
	CFG* cfg;
	Int idx;

	CGD_ASSERT(addr != 0);

	idx = cfg_hash_idx(addr, cfgs.size);
	cfg = cfgs.table[idx];

	while (cfg) {
		if (cfg->addr == addr)
			break;

		cfg = cfg->chain;
	}

	return cfg;
}

static
Bool has_cfg_node(CFG* cfg, CfgNode* node) {
	Int i, count;

	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(node != 0);

	count = CGD_(smart_list_count)(cfg->nodes);
	for (i = 0; i < count; i++) {
		CfgNode* tmp = (CfgNode*) CGD_(smart_list_at)(cfg->nodes, i);
		CGD_ASSERT(tmp != 0);

		if (CGD_(cfgnodes_cmp)(node, tmp))
			return True;
	}

	// Not found.
	return False;
}

static
CfgEdge* find_edge(CfgNode* src, CfgNode* dst) {
	Int i, count;

	CGD_ASSERT(src != 0 && (src->type != CFG_EXIT && src->type != CFG_HALT));
	CGD_ASSERT(dst != 0 && dst->type != CFG_ENTRY);

	CGD_ASSERT(src->info.successors != 0);
	count = CGD_(smart_list_count)(src->info.successors);
	for (i = 0; i < count; i++) {
		CfgEdge* edge = (CfgEdge*) CGD_(smart_list_at)(src->info.successors, i);
		CGD_ASSERT(edge != 0);
		CGD_ASSERT(CGD_(cfgnodes_cmp)(edge->src, src));

		if (CGD_(cfgnodes_cmp)(dst, edge->dst))
			return edge;
	}

	// Not found.
	return 0;
}

static
CfgCall* find_call(CfgNode* node, CFG* call) {
	Int i, count;

	CGD_ASSERT(node != 0);
	CGD_ASSERT(node->type == CFG_BLOCK);

	if (node->data.block->calls) {
		count = CGD_(smart_list_count)(node->data.block->calls);
		for (i = 0; i < count; i++) {
			CfgCall* tmp = (CfgCall*) CGD_(smart_list_at)(node->data.block->calls, i);
			CGD_ASSERT(tmp != 0);

			if (CGD_(cfg_cmp)(tmp->called, call))
				return tmp;
		}
	}

	return 0;
}

static
CfgCall* find_call_with_addr(CfgNode* node, Addr addr) {
	Int i, size;

	CGD_ASSERT(node != 0 && node->type == CFG_BLOCK);
	CGD_ASSERT(addr != 0);

	if (node->data.block->calls) {
		size = CGD_(smart_list_count)(node->data.block->calls);
		for (i = 0; i < size; i++) {
			CfgCall* cfgCall = (CfgCall*) CGD_(smart_list_at)(node->data.block->calls, i);
			CGD_ASSERT(cfgCall != 0);

			if (cfgCall->called->addr == addr)
				return cfgCall;
		}
	}

	return 0;
}

static
CfgSignalHandler* find_signal_handler(CfgNode* node, Int signum) {
	Int i, size;

	CGD_ASSERT(node != 0);
	CGD_ASSERT(node->type == CFG_BLOCK);

	if (node->data.block->sighandlers) {
		size = CGD_(smart_list_count)(node->data.block->sighandlers);
		for (i = 0; i < size; i++) {
			CfgSignalHandler* tmp = (CfgSignalHandler*)
				CGD_(smart_list_at)(node->data.block->sighandlers, i);
			CGD_ASSERT(tmp != 0);

			if (tmp->signum == signum)
				return tmp;
		}
	}

	return 0;
}

static
CfgNode* find_successor_with_addr(CfgNode* node, Addr addr) {
	Int i, size;

	CGD_ASSERT(node != 0);
	CGD_ASSERT(addr != 0);

	size = CGD_(smart_list_count)(node->info.successors);
	for (i = 0; i < size; i++) {
		CfgEdge* edge;
		CfgNode* succ;

		edge = (CfgEdge*) CGD_(smart_list_at)(node->info.successors, i);
		CGD_ASSERT(edge != 0);

		succ = edge->dst;

		// Ignore exit nodes. Entry nodes should never be successor.
		if (succ->type == CFG_EXIT || succ->type == CFG_HALT)
			continue;

		if (CGD_(cfgnode_addr)(succ) == addr)
			return succ;
	}

	return 0;
}

static
Bool add_node2cfg(CFG* cfg, CfgNode* node) {
	if (!has_cfg_node(cfg, node)) {
		// We can only add block or phantom nodes.
		CGD_ASSERT(node->type == CFG_BLOCK ||
				   node->type == CFG_PHANTOM);

		// Add the node to the list of nodes in the CFG.
		CGD_(smart_list_add)(cfg->nodes, node);

		// Mark the CFG as dirty.
		cfg->dirty = True;

		return True;
	}

	return False;
}

static
#if ENABLE_PROFILING
Bool add_edge2nodes(CFG* cfg, CfgNode* src, CfgNode* dst, ULong count) {
#else
Bool add_edge2nodes(CFG* cfg, CfgNode* src, CfgNode* dst) {
#endif
	CfgEdge* edge = find_edge(src, dst);
	if (edge) {
#if ENABLE_PROFILING
		edge->count += count;

		// Mark the CFG as dirty.
		cfg->dirty = True;
#endif

		return False;
	}

	// Create the edge.
	edge = (CfgEdge*) CGD_MALLOC("cgd.cfg.ae2n.1", sizeof(CfgEdge));
	VG_(memset)(edge, 0, sizeof(edge));
	edge->src = src;
	edge->dst = dst;
#if ENABLE_PROFILING
	edge->count = count;
#endif

	// Add the edge to the CFG.
	CGD_(smart_list_add)(cfg->edges, edge);

	// Add the successor.
	CGD_ASSERT(src->info.successors != 0);
	CGD_(smart_list_add)(src->info.successors, edge);

	// Add the predecessor.
	CGD_ASSERT(dst->info.predecessors != 0);
	CGD_(smart_list_add)(dst->info.predecessors, edge);

	// Mark the CFG as dirty.
	cfg->dirty = True;

	return True;
}

static
#if ENABLE_PROFILING
void add_call2node(CFG* cfg, CfgNode* node, CFG* called, ULong count) {
#else
void add_call2node(CFG* cfg, CfgNode* node, CFG* called) {
#endif
	CfgCall* cfgCall = find_call(node, called);
	if (cfgCall) {
#if ENABLE_PROFILING
		cfgCall->count += count;

		// Mark the CFG as dirty.
		cfg->dirty = True;
#endif
	} else {
		CGD_ASSERT(find_successor_with_addr(node, called->addr) == 0);

		cfgCall = (CfgCall*) CGD_MALLOC("cgd.cfg.cac.1", sizeof(CfgCall));
		VG_(memset)(cfgCall, 0, sizeof(CfgCall));
		cfgCall->called = called;
#if ENABLE_PROFILING
		cfgCall->count = count;
#endif

		if (!node->data.block->calls)
			node->data.block->calls = CGD_(new_smart_list)(1);

		CGD_(smart_list_add)(node->data.block->calls, cfgCall);

		// Mark the CFG as dirty.
		cfg->dirty = True;
	}
}

static
#if ENABLE_PROFILING
void add_sighandler2node(CFG* cfg, CfgNode* node, CFG* called, Int signum, ULong count) {
#else
void add_sighandler2node(CFG* cfg, CfgNode* node, CFG* called, Int signum) {
#endif
	CfgSignalHandler* sigHandler = find_signal_handler(node, signum);
	if (sigHandler) {
		CGD_ASSERT(CGD_(cfg_cmp)(sigHandler->handler->called, called));

#if ENABLE_PROFILING
		sigHandler->handler->count += count;

		// Mark the CFG as dirty.
		cfg->dirty = True;
#endif
	} else {
		sigHandler = (CfgSignalHandler*) CGD_MALLOC("cgd.cfg.cssh.1", sizeof(CfgSignalHandler));
		VG_(memset)(sigHandler, 0, sizeof(CfgSignalHandler));

		sigHandler->signum = signum;
		sigHandler->handler = (CfgCall*) CGD_MALLOC("cgd.cfg.cssh.2", sizeof(CfgCall));
		VG_(memset)(sigHandler->handler, 0, sizeof(CfgCall));
		sigHandler->handler->called = called;

#if ENABLE_PROFILING
		sigHandler->handler->count += count;
#endif

		if (!node->data.block->sighandlers)
			node->data.block->sighandlers = CGD_(new_smart_list)(1);

		CGD_(smart_list_add)(node->data.block->sighandlers, sigHandler);

		// Mark the CFG as dirty.
		cfg->dirty = True;
	}
}

static
CfgNode* new_cfgnode(enum CfgNodeType type, Int succs, Int preds) {
	CfgNode* node;

	CGD_ASSERT(succs >= 0);

	node = (CfgNode*) CGD_MALLOC("cgd.cfg.ncn.1", sizeof(CfgNode));
	VG_(memset)(node, 0, sizeof(CfgNode));

	node->id = ++CGD_(stat).distinct_cfg_nodes;
	node->type = type;

	if (succs > 0) {
		node->info.successors = CGD_(new_smart_list)(succs);
	}

	if (preds > 0) {
		node->info.predecessors = CGD_(new_smart_list)(preds);
	}

	return node;
}

static __inline__
CfgBlock* new_block(CfgInstrRef* ref) {
	CfgBlock* block;

	CGD_ASSERT(ref != 0);
	CGD_ASSERT(ref->instr != 0);

	block = (CfgBlock*) CGD_MALLOC("cgd.cfg.nb.1", sizeof(CfgBlock));
	VG_(memset)(block, 0,  sizeof(CfgBlock));

	block->addr = ref->instr->addr;
	block->size = ref->instr->size;

	// Make this instruction the first and last in the block.
	block->instrs.leader = ref;
	block->instrs.tail = ref;
	block->instrs.count = 1;

	// Make all the references in the block orphan of its parent node
	// PS: It will be fixed later.
	block->instrs.tail->node = 0;

	// Follow the instructions chain.
	while (block->instrs.tail->next != 0) {
		// Find the new tail.
		block->instrs.tail = block->instrs.tail->next;

		// Account for this new instruction in the block.
		block->instrs.count++;
		block->size += block->instrs.tail->instr->size;

		// Make the next one orphan too.
		block->instrs.tail->node = 0;
	}

	return block;
}

static
void delete_cfgcall(CfgCall* call) {
	CGD_ASSERT(call != 0);
	CGD_DATA_FREE(call, sizeof(CfgCall));
}

static
void delete_cfgsighandler(CfgSignalHandler* sighandler) {
	CGD_ASSERT(sighandler != 0);
	CGD_ASSERT(sighandler->handler != 0);

	CGD_DATA_FREE(sighandler->handler, sizeof(CfgCall));
	CGD_DATA_FREE(sighandler, sizeof(CfgSignalHandler));
}

static
void delete_block(CfgBlock* block) {
	CfgInstrRef* ref;
	CfgInstrRef* next;

	CGD_ASSERT(block != 0);

	ref = block->instrs.leader;
	while (ref) {
		next = ref->next;
		delete_instr_ref(ref);
		ref = next;
	}

	if (block->calls) {
		CGD_(smart_list_clear)(block->calls, (void (*)(void*)) delete_cfgcall);
		CGD_(delete_smart_list)(block->calls);
	}

	if (block->sighandlers) {
		CGD_(smart_list_clear)(block->sighandlers, (void (*)(void*)) delete_cfgsighandler);
		CGD_(delete_smart_list)(block->sighandlers);
	}

	CGD_DATA_FREE(block, sizeof(CfgBlock));
}

static
void cfgnode_put_block(CFG* cfg, CfgNode* node, CfgBlock* block) {
	CfgInstrRef* ref;
	CfgInstrRef* old;

	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(node != 0);
	CGD_ASSERT(block != 0);
	CGD_ASSERT(node->data.block == 0);

	// Add the block to the node.
	node->data.block = block;

	// Get the first instruction in the block.
	ref = block->instrs.leader;
	CGD_ASSERT(ref != 0);
	CGD_ASSERT(node->data.block->addr == ref->instr->addr);

	// Fix the node in all instructions refs and add them to the cache.
	while (ref) {
		// Update the reference node information.
		CGD_ASSERT(ref->node == 0);
		ref->node = node;

		// Add the reference to the CFG cache and ensure it should not be another ref
		// with the same adddress.
		old = CGD_(smart_hash_put)(cfg->cache.refs, ref, (HWord (*)(void*)) ref_instr_addr);
		CGD_ASSERT(old == 0 || old == ref);

		ref = ref->next;
	}

	// Mark the CFG as dirty.
	cfg->dirty = True;
}

static
void cfgnode_put_phantom(CFG* cfg, CfgNode* node, CfgInstrRef* ref) {
	CfgInstrRef* old;

	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(node != 0);
	CGD_ASSERT(ref != 0);
	CGD_ASSERT(node->data.phantom == 0);

	// Add the block to the node.
	node->data.phantom = ref;
	ref->node = node;
	ref->next = 0;

	// Add the reference to the CFG cache and ensure it should not be another ref
	// with the same adddress.
	old = CGD_(smart_hash_put)(cfg->cache.refs, ref, (HWord (*)(void*)) ref_instr_addr);
	CGD_ASSERT(old == 0 || old == ref);

	// Mark the CFG as dirty.
	cfg->dirty = True;
}

static
void add_ref2node(CFG* cfg, CfgNode* node, CfgInstrRef* ref) {
	CfgInstrRef** last;
	CfgInstrRef* old;

	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(node != 0);
	CGD_ASSERT(node->type == CFG_BLOCK && node->data.block != 0);
	CGD_ASSERT(ref != 0);

	// Get the last instruction reference in the node.
	last = &(node->data.block->instrs.tail);
	CGD_ASSERT(*last != 0 && (*last)->next == 0);
	CGD_ASSERT((*last)->node == node);
	CGD_ASSERT(((*last)->instr->addr + (*last)->instr->size) == ref->instr->addr);

	// Add the last reference.
	(*last)->next = ref;
	ref->node = node;
	ref->next = 0;
	*last = ref;

	// Account for the instruction and its size in the block.
	node->data.block->instrs.count++;
	node->data.block->size += ref->instr->size;

	// Add the reference to the CFG cache and ensure it should not be another ref
	// with the same adddress.
	old = CGD_(smart_hash_put)(cfg->cache.refs, ref, (HWord (*)(void*)) ref_instr_addr);
	CGD_ASSERT(old == 0 || old == ref);

	// Mark the CFG as dirty.
	cfg->dirty = True;
}

static
CfgNode* new_cfgnode_block(CFG* cfg, CfgInstrRef* ref) {
	CfgNode* node;

	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(ref != 0);

	node = new_cfgnode(CFG_BLOCK, 2, 1);
	cfgnode_put_block(cfg, node, new_block(ref));

	// Add the node to the CFG.
	add_node2cfg(cfg, node);

	// Account for block nodes.
	cfg->stats.blocks++;

	return node;
}

static
CfgNode* new_cfgnode_phantom(CFG* cfg, CfgInstrRef* ref) {
	CfgNode* node;

	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(ref != 0);

	node = new_cfgnode(CFG_PHANTOM, 0, 1);
	cfgnode_put_phantom(cfg, node, ref);

	// Add the node to the CFG.
	add_node2cfg(cfg, node);

	// Account for phantom nodes.
	cfg->stats.phantoms++;

	return node;
}

static
CfgNode* cfgnode_exit(CFG* cfg) {
	CGD_ASSERT(cfg != 0);

	if (!cfg->exit) {
		cfg->exit = new_cfgnode(CFG_EXIT, 0, 1);
		CGD_(smart_list_add)(cfg->nodes, cfg->exit);

		// Mark the CFG as dirty.
		cfg->dirty = True;
	}

	return cfg->exit;
}

static
CfgNode* cfgnode_halt(CFG* cfg) {
	CGD_ASSERT(cfg != 0);

	if (!cfg->halt) {
		cfg->halt = new_cfgnode(CFG_HALT, 0, 1);
		CGD_(smart_list_add)(cfg->nodes, cfg->halt);

		// Mark the CFG as dirty.
		cfg->dirty = True;
	}

	return cfg->halt;
}

#if CFG_NODE_CACHE_SIZE > 0
static __inline__
CfgNodeBlockCache* cfgblock_cache(CfgNode* node, Addr addr) {
	CGD_ASSERT(node != 0);

	if (!node->cache.block) {
		Int memsize = CFG_NODE_CACHE_SIZE * sizeof(CfgNodeBlockCache);
		node->cache.block = (CfgNodeBlockCache*) CGD_MALLOC("cgd.cfg.cbc.1", memsize);
		VG_(memset)(node->cache.block, 0, memsize);
	}

	return &(node->cache.block[CFG_NODE_CACHE_INDEX(addr)]);
}

static __inline__
CfgNodePhantomCache* cfgphantom_cache(CfgNode* node, Addr addr) {
	CGD_ASSERT(node != 0);

	if (!node->cache.phantom) {
		Int memsize = CFG_NODE_CACHE_SIZE * sizeof(CfgNodePhantomCache);
		node->cache.phantom = (CfgNodePhantomCache*) CGD_MALLOC("cgd.cfg.cpc.1", memsize);
		VG_(memset)(node->cache.phantom, 0, memsize);
	}

	return &(node->cache.phantom[CFG_NODE_CACHE_INDEX(addr)]);
}

static __inline__
CfgNodeCallCache* cfgcall_cache(CfgNode* node, Addr addr) {
	CGD_ASSERT(node != 0);

	if (!node->cache.call) {
		Int memsize = CFG_NODE_CACHE_SIZE * sizeof(CfgNodeCallCache);
		node->cache.call = (CfgNodeCallCache*) CGD_MALLOC("cgd.cfg.ccc.1", memsize);
		VG_(memset)(node->cache.call, 0, memsize);
	}

	return &(node->cache.call[CFG_NODE_CACHE_INDEX(addr)]);
}
#endif

static
void delete_cfgnode(CfgNode* node) {
	CGD_ASSERT(node != 0);

	switch (node->type) {
		case CFG_BLOCK:
			delete_block(node->data.block);
			break;
		case CFG_PHANTOM:
			delete_instr_ref(node->data.phantom);
			break;
		default:
			break;
	}

	if (node->info.successors) {
		CGD_(smart_list_clear)(node->info.successors, 0);
		CGD_(delete_smart_list)(node->info.successors);
	}

	if (node->info.predecessors) {
		CGD_(smart_list_clear)(node->info.predecessors, 0);
		CGD_(delete_smart_list)(node->info.predecessors);
	}

#if CFG_NODE_CACHE_SIZE > 0
	if (node->cache.block)
		CGD_FREE(node->cache.block);

	if (node->cache.phantom)
		CGD_FREE(node->cache.phantom);

	if (node->cache.call)
		CGD_FREE(node->cache.call);
#endif

	CGD_DATA_FREE(node, sizeof(CfgNode));
}

static
void delete_cfgedge(CfgEdge* edge) {
	CGD_ASSERT(edge != 0);

	CGD_DATA_FREE(edge, sizeof(CfgEdge));
}

static __inline__
Bool ref_is_head(CfgInstrRef* ref) {
	CGD_ASSERT(ref && ref->node && ref->node->type == CFG_BLOCK);
	return ref == ref->node->data.block->instrs.leader;
}

static
Bool remove_edge(CFG* cfg, CfgNode* src, CfgNode* dst) {
	Int i, size;

	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(src != 0);
	CGD_ASSERT(dst != 0);

	size = CGD_(smart_list_count)(cfg->edges);
	for (i = 0; i < size; i++) {
		CfgEdge* edge = (CfgEdge*) CGD_(smart_list_at)(cfg->edges, i);
		CGD_ASSERT(edge != 0);

		if (CGD_(cfgnodes_cmp)(edge->src, src) &&
			CGD_(cfgnodes_cmp)(edge->dst, dst)) {
			Int j, size2;
			CfgEdge* last;

#if ENABLE_PROFILING
			// This edge can only be removed if it was never executed.
			CGD_ASSERT(edge->count == 0);
#endif

			// Remove from the successors list.
			size2 = CGD_(smart_list_count)(src->info.successors);
			for (j = 0; j < size2; j++) {
				CfgEdge* succ_edge = (CfgEdge*)
							CGD_(smart_list_at)(src->info.successors, j);
				CGD_ASSERT(succ_edge != 0);

				if (CGD_(cfgnodes_cmp)(succ_edge->dst, dst)) {
					CGD_ASSERT(succ_edge == edge);

					last = CGD_(smart_list_at)(src->info.successors, (size2 - 1));
					CGD_(smart_list_set)(src->info.successors, j, last);
					CGD_(smart_list_set)(src->info.successors, (size2 - 1), 0);

					break;
				}
			}
			CGD_ASSERT(j < size2); // Did we find it?

			// Remove from the predecessors list.
			size2 = CGD_(smart_list_count)(dst->info.predecessors);
			for (j = 0; j < size2; j++) {
				CfgEdge* pred_edge = (CfgEdge*)
							CGD_(smart_list_at)(dst->info.predecessors, j);
				CGD_ASSERT(pred_edge != 0);

				if (CGD_(cfgnodes_cmp)(pred_edge->src, src)) {
					CGD_ASSERT(pred_edge == edge);

					last = CGD_(smart_list_at)(dst->info.predecessors, (size2 - 1));
					CGD_(smart_list_set)(dst->info.predecessors, j, last);
					CGD_(smart_list_set)(dst->info.predecessors, (size2 - 1), 0);

					break;
				}
			}
			CGD_ASSERT(j < size2); // Did we find it?

			// Remove from the cfg edges.
			last = (CfgEdge*) CGD_(smart_list_at)(cfg->edges, (size - 1));
			CGD_(smart_list_set)(cfg->edges, i, last);
			CGD_(smart_list_set)(cfg->edges, (size - 1), 0);

			return True;
		}
	}

	return False;
}

static
void remove_phantom(CFG* cfg, CfgNode* phantom) {
	CfgInstrRef* ref;
	SmartValue* pos;

	// We can only remove phantom nodes.
	CGD_ASSERT(phantom != 0);
	CGD_ASSERT(phantom->type == CFG_PHANTOM);

	// Remove all edges pointing to it.
	while (CGD_(smart_list_count)(phantom->info.predecessors) > 0) {
		CfgEdge* edge;
		Bool removed;

		edge = (CfgEdge*) CGD_(smart_list_head)(phantom->info.predecessors);
		CGD_ASSERT(edge != 0);
		CGD_ASSERT(edge->dst == phantom);

		removed = remove_edge(cfg, edge->src, phantom);
		CGD_ASSERT(removed);
	}

	// Free the node's memory.
	pos = CGD_(smart_list_find)(cfg->nodes,
			(Bool (*)(void*, void*)) CGD_(cfgnodes_cmp), phantom);
	CGD_ASSERT(pos != 0 && pos->next == 0);
	CGD_(smart_list_set)(cfg->nodes, pos->index, CGD_(smart_list_tail)(cfg->nodes));
	CGD_(smart_list_set)(cfg->nodes, CGD_(smart_list_count)(cfg->nodes) - 1, 0);
	CGD_(smart_list_delete_value)(pos);

	ref = phantom->data.phantom;
	CGD_ASSERT(ref != 0);

	// Remove the reference from the CFG's instruction cache.
	CGD_(smart_hash_remove)(cfg->cache.refs, ref_instr_addr(ref),
			(HWord (*)(void*)) ref_instr_addr);

	delete_cfgnode(phantom);

	CGD_ASSERT(cfg->stats.phantoms > 0);
	cfg->stats.phantoms--;
}

static
CfgInstrRef* cfg_instr_find(CFG* cfg, Addr addr) {
	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(addr != 0);

	return (CfgInstrRef*) CGD_(smart_hash_get)(cfg->cache.refs,
				addr, (HWord (*)(void*)) ref_instr_addr);
}

static
CfgNode* cfgnode_split(CFG* cfg, CfgInstrRef* ref) {
	Int i, size;
#if ENABLE_PROFILING
	ULong count;
#endif
	CfgNode* node;
	CfgNode* pred;
	CfgInstrRef* first;
	CfgInstrRef* last;

	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(ref != 0);

	node = ref->node;
	CGD_ASSERT(!ref_is_head(ref));

	first = last = node->data.block->instrs.leader;
	while (last->next != ref) {
		// Account for the removal of the current instruction
		// (last at this iteration).
		node->data.block->size -= last->instr->size;
		node->data.block->instrs.count--;

		last = last->next;
		CGD_ASSERT(last != 0);
	}

	// Update the node with the new leader.
	node->data.block->addr = ref->instr->addr;
	node->data.block->size -= last->instr->size;
	node->data.block->instrs.leader = ref;
	node->data.block->instrs.count--;

	// Create a new predecessor node with the first reference until the last.
	last->next = 0;
	pred = new_cfgnode_block(cfg, first);

	// The predecessor has naturally a fallthrough to the node.
	pred->info.has_fallthrough = True;

#if ENABLE_PROFILING
	count = 0;
#endif
	CGD_ASSERT(node->info.predecessors != 0);
	size = CGD_(smart_list_count)(node->info.predecessors);
	for (i = 0; i < size; i++) {
		CfgEdge* edge = (CfgEdge*) CGD_(smart_list_at)(node->info.predecessors, i);

		// Move the edge to the predecessor and remove it from the node.
		CGD_ASSERT(edge->dst == node);

		edge->dst = pred;
#if ENABLE_PROFILING
		count += edge->count;
#endif
		CGD_(smart_list_add)(pred->info.predecessors, edge);
		CGD_(smart_list_set)(node->info.predecessors, i, 0);
	}

	// Finally, connect both nodes.
	CGD_ASSERT(CGD_(smart_list_count)(node->info.predecessors) == 0);
#if ENABLE_PROFILING
	add_edge2nodes(cfg, pred, node, count);
#else
	add_edge2nodes(cfg, pred, node);
#endif

	// Return the created predecessor.
	return pred;
}

static
CFG* new_cfg(Addr addr) {
	CFG* cfg;

	CGD_ASSERT(addr != 0);

	cfg = (CFG*) CGD_MALLOC("cgd.cfg.nc.1", sizeof(CFG));
	VG_(memset)(cfg, 0, sizeof(CFG));

	cfg->addr = addr;

	// Create the nodes list.
	cfg->nodes = CGD_(new_smart_list)(3);
	cfg->edges = CGD_(new_smart_list)(3);

	cfg->entry = new_cfgnode(CFG_ENTRY, 1, 0);
	CGD_(smart_list_add)(cfg->nodes, cfg->entry);

	cfg->cache.refs = CGD_(new_smart_hash)(137);

	CGD_(stat).distinct_cfgs++;

	return cfg;
}

static
void delete_cfg(CFG* cfg) {
	CGD_ASSERT(cfg != 0);

	if (cfg->fdesc)
		CGD_(delete_fdesc)(cfg->fdesc);

	CGD_(smart_list_clear)(cfg->edges, (void (*)(void*)) delete_cfgedge);
	CGD_(delete_smart_list)(cfg->edges);

	CGD_(smart_list_clear)(cfg->nodes, (void (*)(void*)) delete_cfgnode);
	CGD_(delete_smart_list)(cfg->nodes);

	CGD_(smart_hash_clear)(cfg->cache.refs, 0);
	CGD_(delete_smart_hash)(cfg->cache.refs);

	CGD_DATA_FREE(cfg, sizeof(CFG));
}

void CGD_(init_cfg_hash)() {
	Int size;

	cfgs.size    = 2137;
	cfgs.entries = 0;

	size = cfgs.size * sizeof(CFG*);
	cfgs.table = (CFG**) CGD_MALLOC("cgd.cfg.ich.1", size);
	VG_(memset)(cfgs.table, 0, size);
}

void CGD_(destroy_cfg_hash)() {
	Int i;

	for (i = 0; i < cfgs.size; i++) {
		CFG* cfg = cfgs.table[i];
		while (cfg) {
			CFG* next = cfg->chain;
			delete_cfg(cfg);
			cfg = next;

			cfgs.entries--;
		}
	}

	CGD_ASSERT(cfgs.entries == 0);

	CGD_FREE(cfgs.table);
	cfgs.table = 0;
}

CFG* CGD_(get_cfg)(Addr addr) {
	CFG* cfg;
	UInt idx;

	cfg = lookup_cfg(addr);
	if (!cfg) {
		/* check fill degree of bb hash table and resize if needed (>80%) */
		cfgs.entries++;
		if (10 * cfgs.entries / cfgs.size > 8)
			resize_cfg_table();

		// Create the cfg.
		cfg = new_cfg(addr);

		/* insert into cfg hash table */
		idx = cfg_hash_idx(addr, cfgs.size);
		cfg->chain = cfgs.table[idx];
		cfgs.table[idx] = cfg;
	}

	return cfg;
}

Addr CGD_(cfg_addr)(CFG* cfg) {
	CGD_ASSERT(cfg != 0);
	return cfg->addr;
}

FunctionDesc* CGD_(cfg_fdesc)(CFG* cfg) {
	CGD_ASSERT(cfg != 0);
	return cfg->fdesc;
}

void CGD_(cfg_build_fdesc)(CFG* cfg) {
	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(cfg->fdesc == 0);

	// Build the function description and update the cfg if it inside main.
	cfg->fdesc = CGD_(new_fdesc)(cfg->addr, True);
}

static
void mark_indirect(CFG* cfg, CfgNode* node) {
	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(node != 0 && node->type == CFG_BLOCK);

	if (!node->data.block->indirect) {
		node->data.block->indirect = True;

		// Account for this indirection.
		cfg->stats.indirects++;
	}
}

Bool CGD_(cfg_is_dirty)(CFG* cfg) {
	CGD_ASSERT(cfg != 0);
	return cfg->dirty;
}

Bool CGD_(cfg_is_visited)(CFG* cfg) {
	CGD_ASSERT(cfg != 0);
	return cfg->visited;
}

void CGD_(cfg_set_visited)(CFG* cfg, Bool visited) {
	CGD_ASSERT(cfg != 0);
	cfg->visited = visited;
}

Bool CGD_(cfg_is_complete)(CFG* cfg) {
	CGD_ASSERT(cfg != 0);
	return cfg->stats.indirects == 0 &&
		   cfg->stats.phantoms == 0;
}

CfgNode* CGD_(cfg_entry_node)(CFG* cfg) {
	CGD_ASSERT(cfg != 0);
	return cfg->entry;
}

CfgNode* CGD_(cfg_exit_node)(CFG* cfg) {
	CGD_ASSERT(cfg != 0);
	return cfg->exit;
}

CfgNode* CGD_(cfg_halt_node)(CFG* cfg) {
	CGD_ASSERT(cfg != 0);
	return cfg->halt;
}

SmartList* CGD_(cfg_nodes)(CFG* cfg) {
	CGD_ASSERT(cfg != 0);
	return cfg->nodes;
}

Bool CGD_(cfg_cmp)(CFG* cfg1, CFG* cfg2) {
	return (cfg1 != 0 && cfg2 != 0 && cfg1->addr == cfg2->addr);
}

Int CGD_(cfgnode_id)(CfgNode* node) {
	CGD_ASSERT(node != 0);
	return node->id;
}

enum CfgNodeType CGD_(cfgnode_type)(CfgNode* node) {
	CGD_ASSERT(node != 0);
	return node->type;
}

const HChar* CGD_(cfgnode_type2str)(enum CfgNodeType type, Bool lowercase) {
	switch (type) {
		case CFG_ENTRY:
			return (lowercase ? "entry" : "Entry");
		case CFG_BLOCK:
			return (lowercase ? "block" : "Block");
		case CFG_PHANTOM:
			return (lowercase ? "phantom" : "Phantom");
		case CFG_EXIT:
			return (lowercase ? "exit" : "Exit");
		case CFG_HALT:
			return (lowercase ? "halt" : "Halt");
		default:
			tl_assert(0);
			return 0;
	}
}

Addr CGD_(cfgnode_addr)(CfgNode* node) {
	CGD_ASSERT(node != 0);

	switch (node->type) {
		case CFG_BLOCK:
			return node->data.block->addr;
		case CFG_PHANTOM:
			return node->data.phantom->instr->addr;
		default:
			tl_assert(0);
			return 0;
	}
}

Int CGD_(cfgnode_size)(CfgNode* node) {
	CGD_ASSERT(node != 0);
	CGD_ASSERT(node->type == CFG_BLOCK);

	return node->data.block->size;
}

SmartList* CGD_(cfgnode_successors)(CfgNode* node) {
	CGD_ASSERT(node != 0);
	return node->info.successors;
}

SmartList* CGD_(cfgnode_predecessors)(CfgNode* node) {
	CGD_ASSERT(node != 0);
	return node->info.predecessors;
}

Bool CGD_(cfgnode_is_visited)(CfgNode* node) {
	CGD_ASSERT(node != 0);
	return node->visited;
}

void CGD_(cfgnode_set_visited)(CfgNode* node, Bool visited) {
	CGD_ASSERT(node != 0);
	node->visited = visited;
}

Bool CGD_(cfgnode_is_indirect)(CfgNode* node) {
	CGD_ASSERT(node != 0);
	return (node->type == CFG_BLOCK && node->data.block->indirect);
}

void CGD_(cfgnode_remove_successor_with_addr)(CFG* cfg, CfgNode* node, Addr addr) {
	Int i, size;

	if (!node->info.successors)
		return;

	size = CGD_(smart_list_count)(node->info.successors);
	for (i = 0; i < size; i++) {
		CfgEdge* edge;
		CfgNode* succ;

		edge = (CfgEdge*) CGD_(smart_list_at)(node->info.successors, i);
		CGD_ASSERT(edge != 0);

		succ = edge->dst;

		// Ignore exit nodes.
		// Entry nodes should never be a successor.
		if (succ->type == CFG_EXIT || succ->type == CFG_HALT)
			continue;

		// Check if we have a successor with this address.
		// If that is the case, remove it if possible.
		if (CGD_(cfgnode_addr)(succ) == addr) {
			// Remove this edge.
			Bool removed = remove_edge(cfg, edge->src, succ);
			CGD_ASSERT(removed);

			CGD_ASSERT(succ->info.predecessors != 0);
			switch (succ->type) {
				case CFG_BLOCK:
					// Check that the block is not orphan.
					CGD_ASSERT(CGD_(smart_list_count)(succ->info.predecessors) > 0);
					break;
				case CFG_PHANTOM:
					// If it is a phantom node without predecessors, than
					// it is safe to remove it.
					if (CGD_(smart_list_count)(succ->info.predecessors) == 0)
						remove_phantom(cfg, succ);

					break;
				default:
					tl_assert(0);
			}

			return;
		}
	}
}

Bool CGD_(cfgnodes_cmp)(CfgNode* node1, CfgNode* node2) {
	return (node1 && node2 && node1->id == node2->id);
}

static __inline__
Bool cfgnode_has_successors(CfgNode* node) {
	CGD_ASSERT(node != 0 && node->type == CFG_BLOCK);
	return CGD_(smart_list_count)(node->info.successors) > 0;
}

static __inline__
Bool cfgnode_has_calls(CfgNode* node) {
	CGD_ASSERT(node != 0 && node->type == CFG_BLOCK);
	return node->data.block->calls != 0 && CGD_(smart_list_count)(node->data.block->calls) > 0;
}

static __inline__
CfgEdge* get_succ_edge(CFG* cfg, CfgNode* from, Addr addr) {
	Int i, size;

	CGD_ASSERT(from->info.successors != 0);
	size = CGD_(smart_list_count)(from->info.successors);
	for (i = 0; i < size; i++) {
		CfgEdge* edge;
		CfgInstrRef* ref;

		edge = (CfgEdge*) CGD_(smart_list_at)(from->info.successors, i);
		CGD_ASSERT(edge != 0);

		switch (edge->dst->type) {
			case CFG_BLOCK:
				ref = edge->dst->data.block->instrs.leader;
				CGD_ASSERT(ref != 0);
				break;
			case CFG_PHANTOM:
				ref = edge->dst->data.phantom;
				CGD_ASSERT(ref != 0);
				break;
			default:
				ref = 0;
				break;
		}

		// Check if the successors head instruction matches the next address.
		if (ref != 0 && ref->instr->addr == addr)
			return edge;
	}

	// Otherwise it is not found.
	return 0;
}

static
void phantom2block(CFG* cfg, CfgNode* node, Int new_size) {
	CfgInstrRef* ref;

	CGD_ASSERT(node != 0 && node->type == CFG_PHANTOM);

	ref = node->data.phantom;

	if (ref->instr->size == 0)
		ref->instr->size = new_size;
	else
		CGD_ASSERT(ref->instr->size == new_size);

	node->type = CFG_BLOCK;
	node->data.block = new_block(ref);
	ref->node = node;

	// Add the successors list.
	CGD_ASSERT(node->info.successors == 0);
	node->info.successors = CGD_(new_smart_list)(2);

	// Update the blocks and phantom stats.
	cfg->stats.blocks++;
	cfg->stats.phantoms--;
}

CfgNode* CGD_(cfgnode_set_block)(CFG* cfg, CfgNode* working, BB* bb, Int group_offset) {
	Addr base_addr, addr;
	UInt bb_idx, size;
	InstrGroupInfo group;
	Int accumulated_size;
	CfgInstrRef* curr;
	CfgInstrRef* next;
#if CFG_NODE_CACHE_SIZE > 0
	CfgNodeBlockCache* cache;
#endif

	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(working != 0);
	CGD_ASSERT(working->type == CFG_ENTRY || working->type == CFG_BLOCK);
	CGD_ASSERT(bb != 0);

	// Get the group.
	CGD_ASSERT(group_offset >= 0 && group_offset < bb->groups_count);
	group = bb->groups[group_offset];

#if CFG_NODE_CACHE_SIZE > 0
	cache = cfgblock_cache(working, group.group_addr);
	cache->addr = group.group_addr;
	cache->size = group.group_size;
#if ENABLE_PROFILING
	cache->count = 0;
#endif // ENABLE_PROFILING
#endif // CFG_NODE_CACHE_SIZE

	// Find the first instruction index.
	bb_idx = group.bb_info.first_instr;
	base_addr = bb_addr(bb);

	// Current instruction reference in the working node,
	// can be any from head to tail. We use it to match the
	// instructions in the group in sequence.
	//
	// A null value is used to indicate that the current
	// instruction in the group must be a successor of
	// the working node. The first instruction of this
	// group must always be a successor, hence the null value.
	curr = 0;

	accumulated_size = 0;
	while (accumulated_size < group.group_size) {
		// If null, find the successor of the working that
		// matches the current instruction in the group.
		// Create, split or transform the node if necessary.
		if (!curr) {
			CfgEdge* edge;

			CGD_ASSERT(bb_idx < bb->instr_count);
			addr = base_addr + bb->instr[bb_idx].instr_offset;
			size = bb->instr[bb_idx].instr_size;

			// First check if we have a successor with this address already.
			// Although this conditional block it is not required for the
			// correctness of the algorithm (it is also covered by the next
			// conditional statement), it is considerably faster overall
			// to search the list first and then check the hash if necessary.
			if ((edge = get_succ_edge(cfg, working, addr))) {
				// The successor may be a phantom node, in this case convert to a block.
				if (edge->dst->type == CFG_PHANTOM)
					phantom2block(cfg, edge->dst, size);

				// Use it as the new working node.
				CGD_ASSERT(edge->dst->type == CFG_BLOCK);
				next = edge->dst->data.block->instrs.leader;
#if ENABLE_PROFILING
				edge->count++;
#endif
				working = edge->dst;
			// If it is not a direct successor, check if there is a instruction
			// with this address already exists in the CFG in some block.
			// This block will be a successor of the working node.
			} else if ((next = cfg_instr_find(cfg, addr))) {
				// If the next node is a phantom, convert to a block node.
				if (next->node->type == CFG_PHANTOM)
					phantom2block(cfg, next->node, size);
				// If the instruction is not the first in the block, split it.
				else if (!ref_is_head(next))
					cfgnode_split(cfg, next);

				// Connect the working block to this and make it the current
				// working node.
				CGD_ASSERT(ref_is_head(next));
#if ENABLE_PROFILING
				add_edge2nodes(cfg, working, next->node, 1);
#else
				add_edge2nodes(cfg, working, next->node);
#endif
				working = next->node;
			// In this case, the instruction is new and we can:
			// (1) append it to the working node (if possible); or
			// (2) create a new block with it as the head instruction that
			// will be connected to the working. This block will be the
			// new working node.
			} else {
				next = new_instr_ref(CGD_(get_instr)(addr, size));
				// Append the instruction if possible.
				if (bb_idx > group.bb_info.first_instr && working->type == CFG_BLOCK &&
					!cfgnode_has_successors(working) && !cfgnode_has_calls(working)) {
					CGD_ASSERT((working->data.block->instrs.tail->instr->addr +
							working->data.block->instrs.tail->instr->size) == addr);
					add_ref2node(cfg, working, next);
				// Create a new block, connect the working to it and
				// make it the new working node.
				} else {
					CfgNode* node = new_cfgnode_block(cfg, next);
#if ENABLE_PROFILING
					add_edge2nodes(cfg, working, node, 1);
#else
					add_edge2nodes(cfg, working, node);
#endif
					working = node;
				}
			}

			// Now, this will be our current instruction.
			curr = next;
		}

		// Try to process the whole block if possible.
		if (ref_is_head(curr) &&
				((accumulated_size + working->data.block->size) <= group.group_size)) {
			accumulated_size += working->data.block->size;
			bb_idx += working->data.block->instrs.count;

			curr = 0;
		// Otherwise, process instruction by instruction in the block.
		} else {
			do {
				CGD_ASSERT(bb_idx < bb->instr_count);
				addr = base_addr + bb->instr[bb_idx].instr_offset;
				size = bb->instr[bb_idx].instr_size;

				CGD_ASSERT(curr->instr->addr == addr);
				CGD_ASSERT(curr->instr->size == size);

				accumulated_size += size;
				bb_idx++;

				curr = curr->next;
			} while (curr && accumulated_size < group.group_size);
		}
	}
	CGD_ASSERT(accumulated_size == group.group_size);

	// If there is a current instruction, this means we didn't reach
	// the end of block, thus a split is required.
	if (curr) {
		working = cfgnode_split(cfg, curr);

#if ENABLE_PROFILING
		{
			// Ugly hack because this whole block was already
			// accounted for. So, decrease the edge count here
			// since it will be added back in the next iteration.
			CfgEdge* edge = find_edge(working, curr->node);
			CGD_ASSERT(edge != 0);
			edge->count--;
		}
#endif
	}

#if CFG_NODE_CACHE_SIZE > 0
	cache->working = working;
#endif

	return working;
}

void CGD_(cfgnode_set_phantom)(CFG* cfg, CfgNode* working, Addr to,
			BBJumpKind jmpkind, Bool indirect) {
	CfgInstrRef* next;
#if CFG_NODE_CACHE_SIZE > 0
	CfgNodePhantomCache* cache;
#endif

	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(working != 0);
	CGD_ASSERT(working->type == CFG_BLOCK);

#if CFG_NODE_CACHE_SIZE > 0
	cache = cfgphantom_cache(working, to);
	cache->addr = to;
	cache->indirect = indirect;
#endif

	switch (jmpkind) {
		case bjk_None:
			break;
		case bjk_Jump:
			if (indirect) {
				// Mark the indirection and ignore the rest of code,
				// since we won't be able to create a phantom node for it.
				mark_indirect(cfg, working);
				return;
			}

			break;
		case bjk_Call:
		case bjk_Return:
			// Calls and returns are handled somewhere else, so we
			// can just skip them here.
			return;
		default:
			tl_assert(0);
	}

	// If we got here it is because we know the target address
	// and it is not an indirection.
	CGD_ASSERT(to != 0);
	CGD_ASSERT(indirect == False);

	// Ignore the phantom node if there is a call to this address.
	if (find_call_with_addr(working, to))
		return;

	// If it does not exist, take some actions.
	if (!get_succ_edge(cfg, working, to)) {
		// If there is no next instruction, check if the address
		// is already present in another part of the code.
		if ((next = cfg_instr_find(cfg, to))) {
			// Check if we need to split it: only if it is
			// a block node and the instruction is not the first.
			if (next->node->type == CFG_BLOCK && !ref_is_head(next))
				// Split the block and get the new reference.
				cfgnode_split(cfg, next);
		} else {
			// If the instruction is new, we need to create
			// a phantom node for it.
			next = new_instr_ref(CGD_(get_instr)(to, 0));
			new_cfgnode_phantom(cfg, next);
		}

		// Connect the nodes.
#if ENABLE_PROFILING
		add_edge2nodes(cfg, working, next->node, 0);
#else
		add_edge2nodes(cfg, working, next->node);
#endif
	}
}

void CGD_(cfgnode_set_call)(CFG* cfg, CfgNode* working, CFG* called, Bool indirect) {
#if CFG_NODE_CACHE_SIZE > 0
	CfgNodeCallCache* cache;
#endif

	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(working != 0);
	CGD_ASSERT(working->type == CFG_BLOCK);
	CGD_ASSERT(called != 0);

#if CFG_NODE_CACHE_SIZE > 0
	cache = cfgcall_cache(working, called->addr);
	cache->called = called;
	cache->indirect = indirect;
#if ENABLE_PROFILING
	cache->count = 0;
#endif // ENABLE_PROFILING
#endif // CFG_NODE_CACHE_SIZE

	if (indirect)
		mark_indirect(cfg, working);

#if ENABLE_PROFILING
	add_call2node(cfg, working, called, 1);
#else
	add_call2node(cfg, working, called);
#endif
}

void CGD_(cfgnode_set_signal_handler)(CFG* cfg, CfgNode* working, CFG* called, Int signum) {
	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(working != 0);
	CGD_ASSERT(working->type == CFG_BLOCK);
	CGD_ASSERT(called != 0);
	CGD_ASSERT(signum > 0);

#if ENABLE_PROFILING
	add_sighandler2node(cfg, working, called, signum, 1);
#else
	add_sighandler2node(cfg, working, called, signum);
#endif
}

CfgNode* CGD_(cfgnode_set_exit)(CFG* cfg, CfgNode* working) {
	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(working != 0);
	CGD_ASSERT(working->type == CFG_BLOCK);

#if CFG_NODE_CACHE_SIZE > 0
	working->cache.exit.enabled = True;
#if ENABLE_PROFILING
	working->cache.exit.count = 0;
#endif // ENABLE_PROFILING
#endif // CFG_NODE_CACHE_SIZE

	// Add the node if it is does not exist yet.
#if ENABLE_PROFILING
	add_edge2nodes(cfg, working, cfgnode_exit(cfg), 1);
#else
	add_edge2nodes(cfg, working, cfgnode_exit(cfg));
#endif
	return cfg->exit;
}

CfgNode* CGD_(cfgnode_set_halt)(CFG* cfg, CfgNode* working) {
	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(working != 0);
	CGD_ASSERT(working->type == CFG_BLOCK);

	// Add the node if it is does not exist yet.
#if ENABLE_PROFILING
	add_edge2nodes(cfg, working, cfgnode_halt(cfg), 1);
#else
	add_edge2nodes(cfg, working, cfgnode_halt(cfg));
#endif

	return cfg->halt;
}

void CGD_(clean_visited_cfgnodes)(CFG* cfg) {
	Int i, size;

	CGD_ASSERT(cfg != 0);

	size = CGD_(smart_list_count)(cfg->nodes);
	for (i = 0; i < size; i++) {
		CfgNode* node = (CfgNode*) CGD_(smart_list_at)(cfg->nodes, i);
		CGD_ASSERT(node != 0);

		node->visited = False;
	}
}

static
void cfgnode_merge(CFG* cfg, CfgEdge* edge) {
	Int i, size;
	CfgBlock* block;
	CfgInstrRef* ref;

	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(edge != 0);
	CGD_ASSERT(edge->src->type == CFG_BLOCK && edge->dst->type == CFG_BLOCK);

	block = edge->src->data.block;
	CGD_ASSERT(block != 0);

	ref = edge->dst->data.block->instrs.leader;
	CGD_ASSERT((block->instrs.tail->instr->addr +
		block->instrs.tail->instr->size) == ref->instr->addr);
	block->instrs.tail->next = ref;

	// Fix the reference nodes and update block count.
	while (ref) {
		block->size += ref->instr->size;
		ref->node = edge->src;
		ref = ref->next;
	}

	// Update the block instructions.
	block->instrs.tail = edge->dst->data.block->instrs.tail;
	block->instrs.count += edge->dst->data.block->instrs.count;
	VG_(memset)(&(edge->dst->data.block->instrs), 0, sizeof(block->instrs));

	// Move the calls.
	if (block->calls) {
		CGD_ASSERT(CGD_(smart_list_is_empty)(block->calls));
		CGD_(delete_smart_list)(block->calls);
		block->calls = 0;
	}
	block->calls = edge->dst->data.block->calls;
	edge->dst->data.block->calls = 0;

	// Move the indirect flag.
	CGD_ASSERT(block->indirect == False);
	block->indirect = edge->dst->data.block->indirect;
	edge->dst->data.block->indirect = False;

	// Move the fallthrough flag.
	edge->src->info.has_fallthrough = edge->dst->info.has_fallthrough;
	edge->dst->info.has_fallthrough = False;

	// Move the successors of the node to its predecessor and fix edges.
	CGD_(smart_list_clear)(edge->src->info.successors, 0);
	size = CGD_(smart_list_count)(edge->dst->info.successors);
	for (i = 0; i < size; i++) {
		CfgEdge* tmp = (CfgEdge*) CGD_(smart_list_at)(edge->dst->info.successors, i);
		CGD_ASSERT(tmp != 0);

		tmp->src = edge->src;
		CGD_(smart_list_set)(edge->dst->info.successors, i, 0);
		CGD_(smart_list_add)(edge->src->info.successors, tmp);
	}

	// Remove edge.
	CGD_ASSERT(CGD_(smart_list_count)(edge->dst->info.predecessors) == 1);
	CGD_(smart_list_clear)(edge->dst->info.predecessors, 0);
	size = CGD_(smart_list_count)(cfg->edges);
	for (i = 0; i < size; i++) {
		CfgEdge* tmp = (CfgEdge*) CGD_(smart_list_at)(cfg->edges, i);
		CGD_ASSERT(tmp != 0);

		if (tmp == edge) {
			CGD_(smart_list_set)(cfg->edges, i, CGD_(smart_list_tail)(cfg->edges));
			CGD_(smart_list_set)(cfg->edges, (size - 1), 0);
			break;
		}
	}
	CGD_ASSERT(i < size);

	// Remove node.
	size = CGD_(smart_list_count)(cfg->nodes);
	for (i = 0; i < size; i++) {
		CfgNode* tmp = (CfgNode*) CGD_(smart_list_at)(cfg->nodes, i);
		CGD_ASSERT(tmp != 0);

		if (tmp == edge->dst) {
			CGD_(smart_list_set)(cfg->nodes, i, CGD_(smart_list_tail)(cfg->nodes));
			CGD_(smart_list_set)(cfg->nodes, (size - 1), 0);
			break;
		}
	}
	CGD_ASSERT(i < size);

	// Free the resources.
	cfg->stats.blocks--;
	delete_cfgnode(edge->dst);
	delete_cfgedge(edge);
}

void CGD_(fix_cfg)(CFG* cfg) {
	Int i, size;

	i = 0;
	size = CGD_(smart_list_count)(cfg->nodes);
	while (i < size) {
		CfgNode* node = (CfgNode*) CGD_(smart_list_at)(cfg->nodes, i);
		CGD_ASSERT(node != 0);

		// Check if it can be merged.
		if (node->type == CFG_BLOCK &&
			CGD_(smart_list_count)(node->info.predecessors) == 1) {
			CfgEdge* edge = (CfgEdge*) CGD_(smart_list_head)(node->info.predecessors);
			CGD_ASSERT(edge != 0);

			if (edge->src->type == CFG_BLOCK && edge->src->info.has_fallthrough &&
				CGD_(smart_list_count)(edge->src->info.successors) == 1) {
				cfgnode_merge(cfg, edge);

				--size;
				continue;
			}
		}

		i++;
	}
}

void CGD_(check_cfg)(CFG* cfg) {
	Int i, j, size, size2;
	Int indirects;
#if ENABLE_PROFILING
	ULong leaving;
#endif

	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(cfg->exit != 0 || cfg->halt != 0);

	indirects = 0;
#if ENABLE_PROFILING
	leaving = 0;
#endif

	size = CGD_(smart_list_count)(cfg->nodes);
	for (i = 0; i < size; i++) {
		struct {
			int size;
#if ENABLE_PROFILING
			ULong count;
#endif
		} in, out;
		CfgNode* node;
		CfgEdge* edge;

		node = (CfgNode*) CGD_(smart_list_at)(cfg->nodes, i);
		CGD_ASSERT(node != 0);

		in.size = node->info.predecessors ?
						CGD_(smart_list_count)(node->info.predecessors) : 0;
#if ENABLE_PROFILING
		in.count = 0;
		for (j = 0; j < in.size; j++) {
			edge = (CfgEdge*) CGD_(smart_list_at)(node->info.predecessors, j);
			CGD_ASSERT(edge != 0);

			in.count += edge->count;
		}
#endif

		out.size = node->info.successors ?
						CGD_(smart_list_count)(node->info.successors) : 0;
#if ENABLE_PROFILING
		out.count = 0;
		for (j = 0; j < out.size; j++) {
			edge = (CfgEdge*) CGD_(smart_list_at)(node->info.successors, j);
			CGD_ASSERT(edge != 0);

			out.count += edge->count;
		}
#endif

		switch (node->type) {
			case CFG_ENTRY:
				{
					CfgInstrRef* ref;

					// An entry node has no predecessors and only a single successor.
					CGD_ASSERT(in.size == 0);
					CGD_ASSERT(out.size == 1);
#if ENABLE_PROFILING
					CGD_ASSERT(in.count == 0);
					CGD_ASSERT(out.count == cfg->stats.execs);
#endif

					edge = (CfgEdge*) CGD_(smart_list_at)(node->info.successors, 0);
					CGD_ASSERT(edge != 0);
					CGD_ASSERT(edge->dst->type == CFG_BLOCK);

					// We must have at least one instruction per block.
					CGD_ASSERT(edge->dst->data.block->instrs.count > 0);

					// The first instruction address must match the cfg address.
					ref = edge->dst->data.block->instrs.leader;
					CGD_ASSERT(ref != 0);
					CGD_ASSERT(cfg->addr == ref->instr->addr);
				}

				break;
			case CFG_EXIT:
			case CFG_HALT:
				// And exit/halt node must have at least one predecessor and no successors.
				CGD_ASSERT(in.size > 0);
				CGD_ASSERT(out.size == 0);

#if ENABLE_PROFILING
				CGD_ASSERT(out.count == 0);
				leaving += in.count;
#endif

				break;
			case CFG_BLOCK:
				{
					Int total, count;
					CfgInstrRef* ref;
					CfgBlock* block;

					// A block node must have at least one predecessor and one successor.
					CGD_ASSERT(in.size > 0);
					CGD_ASSERT(out.size > 0);

#if ENABLE_PROFILING
					// The in and out degree of edges must match.
					CGD_ASSERT(in.count == out.count);
#endif

					block = node->data.block;
					CGD_ASSERT(block != 0);

					count = 0;
					total = 0;
					ref = block->instrs.leader;
					while (ref) {
						if (ref->next) {
							CGD_ASSERT((ref->instr->addr + ref->instr->size) == ref->next->instr->addr);
						} else {
							CGD_ASSERT(block->instrs.tail == ref);
						}

						count++;
						total += ref->instr->size;

						ref = ref->next;
					}
					CGD_ASSERT(count == block->instrs.count);
					CGD_ASSERT(total == block->size);

					ref = block->instrs.tail;

					if (block->calls) {
						size2 = CGD_(smart_list_count)(block->calls);
						for (j = 0; j < size2; j++) {
							CfgCall* cfgCall = (CfgCall*) CGD_(smart_list_at)(block->calls, j);
							CGD_ASSERT(cfgCall != 0);

							CGD_ASSERT(find_successor_with_addr(node, cfgCall->called->addr) == 0);

							// Note: we should not match the calls count with the successors counts
							// because, with call emulations, some jumps may be taken that only
							// later was identified as calls (ex.: late binding).
						}
					}

					if (block->indirect)
						indirects++;
				}

				break;
			case CFG_PHANTOM:
				// A phantom must have at least one predecessor and no successors.
				CGD_ASSERT(in.size > 0);
				CGD_ASSERT(out.size == 0);

#if ENABLE_PROFILING
				CGD_ASSERT(in.count == 0);
				CGD_ASSERT(out.count == 0);
#endif

				// The phantom address must not be 0.
				CGD_ASSERT(node->data.phantom != 0);

				break;
			default:
				tl_assert(0);
		}
	}

	CGD_ASSERT(CGD_(smart_list_count)(cfg->nodes) ==
			(1 + (cfg->exit ? 1 : 0) + (cfg->halt ? 1 : 0) + cfg->stats.blocks + cfg->stats.phantoms));
	CGD_ASSERT(indirects == cfg->stats.indirects);
#if ENABLE_PROFILING
	CGD_ASSERT(leaving == cfg->stats.execs);
#endif

	// After checking, set the CFG as not dirty.
	cfg->dirty = False;
}

static
void fprintf_escape(VgFile* out, const HChar* str) {
	while (*str) {
		if (*str == '<' || *str == '>')
			VG_(fprintf)(out, "\\");

		VG_(fprintf)(out, "%c", *str);
		str++;
	}
}

static
void fprint_cfg(VgFile* out, CFG* cfg, Bool detailed) {
	Int i, size;
	Int j, size2;
	Int unknown;

	CGD_ASSERT(out != 0);
	CGD_ASSERT(cfg != 0);

	VG_(fprintf)(out, "digraph \"0x%lx\" {\n", cfg->addr);

	VG_(fprintf)(out, "  label = \"0x%lx (", cfg->addr);
	if (!cfg->fdesc)
		CGD_(cfg_build_fdesc)(cfg);
	if (cfg->fdesc) {
		CGD_(fprint_fdesc)(out, cfg->fdesc);
	} else {
		VG_(fprintf)(out, "unknown");
	}
	VG_(fprintf)(out, ")\"\n");
	VG_(fprintf)(out, "  labelloc = \"t\"\n");
	VG_(fprintf)(out, "  node[shape=record]\n\n");

	unknown = 1;
	size = CGD_(smart_list_count)(cfg->nodes);
	for (i = 0; i < size; i++) {
		CfgNode* node;

		node = (CfgNode*) CGD_(smart_list_at)(cfg->nodes, i);
		CGD_ASSERT(node != 0);

		if (node->type == CFG_ENTRY) {
			VG_(fprintf)(out, "  %s [label=\"\",width=0.3,height=0.3,shape=circle,fillcolor=black,style=filled]\n",
					CGD_(cfgnode_type2str)(CFG_ENTRY, False));
		} else if (node->type == CFG_EXIT) {
			VG_(fprintf)(out, "  %s [label=\"\",width=0.3,height=0.3,shape=circle,fillcolor=black,style=filled,peripheries=2]\n",
					CGD_(cfgnode_type2str)(CFG_EXIT, False));
		} else if (node->type == CFG_HALT) {
			VG_(fprintf)(out, "  %s [label=\"\",width=0.3,height=0.3,shape=square,fillcolor=black,style=filled,peripheries=2]\n",
					CGD_(cfgnode_type2str)(CFG_HALT, False));
		} else if (node->type == CFG_BLOCK) {
			VG_(fprintf)(out, "  \"0x%lx\" [label=\"{\n", CGD_(cfgnode_addr)(node));
			VG_(fprintf)(out, "     0x%lx [%d]\\l\n",
					node->data.block->addr, node->data.block->size);

			if (detailed) {
				CfgInstrRef* ref;

				VG_(fprintf)(out, "     | [instrs]\\l\n");

				ref = node->data.block->instrs.leader;
				CGD_ASSERT(ref != 0);

				while (ref) {
					VG_(fprintf)(out, "     &nbsp;&nbsp;0x%lx \\<+%d\\>: ",
							ref->instr->addr, ref->instr->size);

					if (ref->instr->name)
						fprintf_escape(out, ref->instr->name);
					else
						VG_(fprintf)(out, "???");

					VG_(fprintf)(out, "\\l\n");

					ref = ref->next;
				}
			}

			if (node->data.block->calls) {
				VG_(fprintf)(out, "     | [calls]\\l\n");

				size2 = CGD_(smart_list_count)(node->data.block->calls);
				for (j = 0; j < size2; j++) {
					CfgCall* cfgCall;
					HChar* desc;

					cfgCall = (CfgCall*) CGD_(smart_list_at)(node->data.block->calls, j);
					CGD_ASSERT(cfgCall != 0);

					VG_(fprintf)(out, "     &nbsp;&nbsp;0x%lx ", cfgCall->called->addr);

#if ENABLE_PROFILING
					if (cfgCall->count > 0)
						VG_(fprintf)(out, "\\{%llu\\} ", cfgCall->count);
#endif

					VG_(fprintf)(out, "(");
					desc = CGD_(fdesc2str)(cfgCall->called->fdesc);
					fprintf_escape(out, desc);
					CGD_FREE(desc);
					VG_(fprintf)(out, ")\\l\n");
				}
			}

			if (node->data.block->sighandlers) {
				VG_(fprintf)(out, "     | [signals]\\l\n");

				size2 = CGD_(smart_list_count)(node->data.block->sighandlers);
				for (j = 0; j < size2; j++) {
					CfgSignalHandler* cfgSighandler;
					HChar* desc;

					cfgSighandler = (CfgSignalHandler*) CGD_(smart_list_at)(node->data.block->sighandlers, j);
					CGD_ASSERT(cfgSighandler != 0);

					VG_(fprintf)(out, "     &nbsp;&nbsp;%02d: 0x%lx ", cfgSighandler->signum,
						cfgSighandler->handler->called->addr);

#if ENABLE_PROFILING
					if (cfgSighandler->handler->count > 0)
						VG_(fprintf)(out, "\\{%llu\\} ", cfgSighandler->handler->count);
#endif

					VG_(fprintf)(out, "(");
					desc = CGD_(fdesc2str)(cfgSighandler->handler->called->fdesc);
					fprintf_escape(out, desc);
					CGD_FREE(desc);
					VG_(fprintf)(out, ")\\l\n");
				}
			}

			VG_(fprintf)(out, "  }\"]\n");
		} else if (node->type == CFG_PHANTOM) {
			VG_(fprintf)(out, "  \"0x%lx\" [label=\"{\n", CGD_(cfgnode_addr)(node));
			VG_(fprintf)(out, "     0x%lx\\l\n",
					node->data.phantom->instr->addr);
			VG_(fprintf)(out, "  }\", style=dashed]\n");
		} else {
			tl_assert(0);
		}

		if (CGD_(cfgnode_is_indirect)(node)) {
			VG_(fprintf)(out, "  \"Unknown%d\" [label=\"?\", shape=none]\n", unknown);
			VG_(fprintf)(out, "  \"0x%lx\" -> \"Unknown%d\" [style=dashed]\n",
					CGD_(cfgnode_addr)(node), unknown);
			unknown++;
		}
	}

	size = CGD_(smart_list_count)(cfg->edges);
	for (i = 0; i < size; i++) {
		CfgEdge* edge = (CfgEdge*) CGD_(smart_list_at)(cfg->edges, i);
		CGD_ASSERT(edge != 0);

		if (edge->src->type == CFG_ENTRY)
			VG_(fprintf)(out, "  %s -> ", CGD_(cfgnode_type2str)(edge->src->type, False));
		else
			VG_(fprintf)(out, "  \"0x%lx\" -> ", CGD_(cfgnode_addr)(edge->src));

		if (edge->dst->type == CFG_EXIT || edge->dst->type == CFG_HALT)
			VG_(fprintf)(out, "%s", CGD_(cfgnode_type2str)(edge->dst->type, False));
		else
			VG_(fprintf)(out, "\"0x%lx\"", CGD_(cfgnode_addr)(edge->dst));

#if ENABLE_PROFILING
		VG_(fprintf)(out, " [label=\" %llu\"]", edge->count);
#endif

		VG_(fprintf)(out, "\n");
	}

	VG_(fprintf)(out, "}\n");
}

void CGD_(fprint_cfg)(VgFile* out, CFG* cfg) {
	fprint_cfg(out, cfg, False);
}

void CGD_(fprint_detailed_cfg)(VgFile* out, CFG* cfg) {
	fprint_cfg(out, cfg, True);
}

static
void write_cfg(CFG* cfg) {
	Int i, size;
	Int j, size2;

	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(fp != 0);

	if (!cfg->fdesc)
		CGD_(cfg_build_fdesc)(cfg);

	VG_(fprintf)(fp, "[cfg 0x%lx", cfg->addr);
#if ENABLE_PROFILING
	if (cfg->stats.execs > 0)
		VG_(fprintf)(fp, ":%llu", cfg->stats.execs);
#endif
	VG_(fprintf)(fp, " \"");

	if (cfg->fdesc)
		CGD_(fprint_fdesc)(fp, cfg->fdesc);
	else
		VG_(fprintf)(fp, "unknown");
	VG_(fprintf)(fp, "\" %s]\n", (CGD_(cfg_is_complete)(cfg) ? "true" : "false"));

	size = CGD_(smart_list_count)(cfg->nodes);
	for (i = 0; i < size; i++) {
		CfgNode* node;
		CfgInstrRef* ref;

		node = (CfgNode*) CGD_(smart_list_at)(cfg->nodes, i);
		CGD_ASSERT(node != 0);

		// We only write block nodes.
		if (node->type != CFG_BLOCK)
			continue;

		VG_(fprintf)(fp, "[node 0x%lx 0x%lx %d ", cfg->addr,
			node->data.block->addr, node->data.block->size);

		ref = node->data.block->instrs.leader;
		CGD_ASSERT(ref != 0);

		VG_(fprintf)(fp, "[");
		while (ref) {
			VG_(fprintf)(fp, "%d", ref->instr->size);

			if (ref->next)
				VG_(fprintf)(fp, " ");

			ref = ref->next;
		}
		VG_(fprintf)(fp, "] ");

		VG_(fprintf)(fp, "[");
		if (node->data.block->calls) {
			size2 = CGD_(smart_list_count)(node->data.block->calls);
			for (j = 0; j < size2; j++) {
				CfgCall* cfgCall = (CfgCall*) CGD_(smart_list_at)(node->data.block->calls, j);
				CGD_ASSERT(cfgCall != 0);

				if (j > 0)
					VG_(fprintf)(fp, " ");

				VG_(fprintf)(fp, "0x%lx", cfgCall->called->addr);
#if ENABLE_PROFILING
				if (cfgCall->count > 0)
					VG_(fprintf)(fp, ":%llu", cfgCall->count);
#endif
			}
		}
		VG_(fprintf)(fp, "] ");

		VG_(fprintf)(fp, "[");
		if (node->data.block->sighandlers) {
			size2 = CGD_(smart_list_count)(node->data.block->sighandlers);
			for (j = 0; j < size2; j++) {
				CfgSignalHandler* cfgSighandler;

				if (j > 0)
					VG_(fprintf)(fp, " ");

				cfgSighandler = (CfgSignalHandler*) CGD_(smart_list_at)(node->data.block->sighandlers, j);
				CGD_ASSERT(cfgSighandler != 0);

				VG_(fprintf)(fp, "%d->0x%lx", cfgSighandler->signum, cfgSighandler->handler->called->addr);

#if ENABLE_PROFILING
				if (cfgSighandler->handler->count > 0)
					VG_(fprintf)(fp, ":%llu", cfgSighandler->handler->count);
#endif
			}
		}
		VG_(fprintf)(fp, "] ");

		VG_(fprintf)(fp, "%s ", node->data.block->indirect ? "true" : "false");

		VG_(fprintf)(fp, "[");
		size2 = CGD_(smart_list_count)(node->info.successors);
		for (j = 0; j < size2; j++) {
			CfgEdge* edge;

			edge = (CfgEdge*) CGD_(smart_list_at)(node->info.successors, j);
			CGD_ASSERT(edge != 0);

			if (j > 0)
				VG_(fprintf)(fp, " ");

			switch (edge->dst->type) {
				case CFG_EXIT:
				case CFG_HALT:
					VG_(fprintf)(fp, "%s", CGD_(cfgnode_type2str)(edge->dst->type, True));
					break;
				case CFG_BLOCK:
				case CFG_PHANTOM:
					VG_(fprintf)(fp, "0x%lx", CGD_(cfgnode_addr)(edge->dst));
					break;
				default:
					tl_assert(0);
			}

#if ENABLE_PROFILING
			if (edge->count > 0)
				VG_(fprintf)(fp, ":%llu", edge->count);
#endif
		}
		VG_(fprintf)(fp, "]");

		VG_(fprintf)(fp, "]\n");
	}
}

void CGD_(write_cfgs)(const HChar* filename) {
	CGD_ASSERT(fp == 0);
	fp = VG_(fopen)(filename, VKI_O_WRONLY|VKI_O_TRUNC, 0);
	if (fp == NULL) {
		fp = VG_(fopen)(filename, VKI_O_CREAT|VKI_O_WRONLY,
				VKI_S_IRUSR|VKI_S_IWUSR);
	}
	CGD_ASSERT(fp != 0);

	VG_(fprintf)(fp, "# [cfg cfg-addr{:invocations} cfg-name is-complete]\n");
	VG_(fprintf)(fp, "# [node cfg-addr node-addr node-size [list of instr-size] [list of cfg-addr{:count}]\n");
	VG_(fprintf)(fp, "#       [list of signal-id->cfg-addr{:count}] is-indirect [list of succ-node{:count}]\n");

	CGD_(forall_cfg)(write_cfg);

	VG_(fclose)(fp);
	fp = 0;
}

static
Bool next_token(Int fd) {
    Int idx, state;
    static Int last = -1;

    idx = 0;
    VG_(memset)(&token, 0, sizeof(token));

    state = 1;
    while (state != 9) {
        Int c;

        CGD_ASSERT(idx >= 0 && idx < ((sizeof(token.text) / sizeof(HChar))-1));
        if (last == -1) {
			Int s;
			HChar tmp;

			s = VG_(read)(fd, &tmp, 1);
			c = s < 1 ? -1 : tmp;
        } else {
			c = last;
			last = -1;
        }

        switch (state) {
            case 1:
				if (c == -1) {
					return False;
				} else if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
					state = 1;
				} else if (c == '0') {
					token.text[idx++] = c;
					token.type = TKN_NUMBER;
					token.data.number = 0;
					state = 2;
				} else if (c >= '1' && c <= '9') {
					token.text[idx++] = c;
					token.type = TKN_NUMBER;
					state = 4;
				} else if (VG_(tolower)(c) >= 'a' && VG_(tolower)(c) <= 'z') {
					token.text[idx++] = VG_(tolower)(c);
					state = 5;
				} else if (c == '[') {
					token.text[idx++] = c;
					token.type = TKN_BRACKET_OPEN;
					state = 9;
				} else if (c == ']') {
					token.text[idx++] = c;
					token.type = TKN_BRACKET_CLOSE;
					state = 9;
				} else if (c == ':') {
					token.text[idx++] = c;
					token.type = TKN_COLON;
					state = 9;
				} else if (c == '\"') {
					token.type = TKN_TEXT;
					state = 6;
				} else if (c == '-') {
					state = 7;
				} else if (c == '#') {
					state = 8;
				} else {
					tl_assert(0);
				}

				break;
            case 2:
				if (VG_(tolower)(c) == 'x') {
					token.text[idx++] = VG_(tolower)(c);
					token.type = TKN_ADDR;
					state = 3;
				} else {
					if (c != -1)
						last = c;

					state = 9;
				}

				break;
            case 3:
				if ((c >= '0' && c <= '9') ||
					(VG_(tolower)(c) >= 'a' && VG_(tolower)(c) <= 'f')) {
					token.text[idx++] = VG_(tolower)(c);
					state = 3;
				} else {
					token.data.addr = VG_(strtoull16)(token.text, 0);

					if (c != -1)
						last = c;

					state = 9;
				}

				break;
            case 4:
				if (c >= '0' && c <= '9') {
					token.text[idx++] = c;
					state = 4;
				} else {
					token.data.number = VG_(strtoull10)(token.text, 0);

					if (c != -1)
						last = c;

					state = 9;
				}

				break;
            case 5:
				if (VG_(tolower)(c) >= 'a' && VG_(tolower)(c) <= 'z') {
					token.text[idx++] = VG_(tolower)(c);
					state = 5;
				} else {
					if (VG_(strcmp)(token.text, "cfg") == 0) {
						token.type = TKN_CFG;
					} else if (VG_(strcmp)(token.text, "node") == 0) {
						token.type = TKN_NODE;
					} else if (VG_(strcmp)(token.text, "exit") == 0) {
						token.type = TKN_EXIT;
					} else if (VG_(strcmp)(token.text, "halt") == 0) {
						token.type = TKN_HALT;
					} else if (VG_(strcmp)(token.text, "true") == 0) {
						token.type = TKN_BOOL;
						token.data.bool = True;
					} else if (VG_(strcmp)(token.text, "false") == 0) {
						token.type = TKN_BOOL;
						token.data.bool = False;
					} else {
						tl_assert(0);
					}

					if (c != -1)
						last = c;

					state = 9;
				}

				break;
			case 6:
				if (c != -1) {
					if (c == '\"')
						state = 9;
					else {
						token.text[idx++] = c;
						state = 6;
					}
				} else {
					tl_assert(0);
				}

				break;
			case 7:
				if (c == '>') {
					token.type = TKN_ARROW;
					token.text[idx++] = c;
					state = 9;
				} else {
					tl_assert(0);
				}

				break;
			case 8:
				if (c == -1) {
					return False;
				} else {
					if (c == '\n')
						state = 1;
					else
						state = 8;
				}

				break;
			default:
				tl_assert(0);
		}
	}

    return True;
}

void CGD_(read_cfgs)(Int fd) {
	Bool has;
	CFG* cfg;
	CfgNode* node;
	CfgInstrRef* ref;
	Addr addr;
	Int block_size;
	Int instr_size;

	while (next_token(fd)) {
		CGD_ASSERT(token.type == TKN_BRACKET_OPEN);

		has = next_token(fd);
		CGD_ASSERT(has);

		switch (token.type) {
			case TKN_CFG:
				has = next_token(fd);
				CGD_ASSERT(has && token.type == TKN_ADDR);

				cfg = CGD_(get_cfg)(token.data.addr);
				CGD_ASSERT(cfg != 0);

				has = next_token(fd);
				CGD_ASSERT(has);
				if (token.type == TKN_COLON) {
					has = next_token(fd);
					CGD_ASSERT(has && token.type == TKN_NUMBER);

#if ENABLE_PROFILING
					if (!CGD_(clo).ignore_profiling)
						cfg->stats.execs = token.data.number;
#endif

					has = next_token(fd);
					CGD_ASSERT(has);
				}

				CGD_ASSERT(has && token.type == TKN_TEXT);
				cfg->fdesc = CGD_(str2fdesc)(token.text);

				has = next_token(fd);
				CGD_ASSERT(has && token.type == TKN_BOOL);

				break;
			case TKN_NODE:
				has = next_token(fd);
				CGD_ASSERT(has && token.type == TKN_ADDR);

				cfg = CGD_(get_cfg)(token.data.addr);
				CGD_ASSERT(cfg != 0);

				has = next_token(fd);
				CGD_ASSERT(has && token.type == TKN_ADDR);
				addr = token.data.addr;

				// Search for the first instruction in the block reference.
				ref = cfg_instr_find(cfg, addr);

				has = next_token(fd);
				CGD_ASSERT(has && token.type == TKN_NUMBER);
				block_size = token.data.number;

				has = next_token(fd);
				CGD_ASSERT(has && token.type == TKN_BRACKET_OPEN);

				has = next_token(fd);
				CGD_ASSERT(has && token.type == TKN_NUMBER);
				instr_size = token.data.number;

				// If the reference exists, then it must be a phantom
				// node and we will convert it to a block node.
				if (ref) {
					CGD_ASSERT(ref->node->type == CFG_PHANTOM);
					node = ref->node;
					phantom2block(cfg, node, instr_size);
				// Otherwise, we will create the block node.
				} else {
					ref = new_instr_ref(CGD_(get_instr)(addr, instr_size));
					node = new_cfgnode_block(cfg, ref);
				}
				CGD_ASSERT(node->type == CFG_BLOCK);

				// If the address match the CFG's addr, then it is the entry block.
				if (addr == cfg->addr) {
#if ENABLE_PROFILING
					add_edge2nodes(cfg, cfg->entry, node, cfg->stats.execs);
#else
					add_edge2nodes(cfg, cfg->entry, node);
#endif
				}

				addr += instr_size;

				has = next_token(fd);
				CGD_ASSERT(has);

				while (token.type == TKN_NUMBER) {
					// This address should not exist yet in the cfg.
					CGD_ASSERT(cfg_instr_find(cfg, addr) == 0);

					instr_size = token.data.number;

					add_ref2node(cfg, node,
							new_instr_ref(CGD_(get_instr)(addr, instr_size)));

					addr += instr_size;

					has = next_token(fd);
					CGD_ASSERT(has);
				}

				CGD_ASSERT(has && token.type == TKN_BRACKET_CLOSE);

				CGD_ASSERT(node->data.block->size == block_size);

				has = next_token(fd);
				CGD_ASSERT(has && token.type == TKN_BRACKET_OPEN);

				has = next_token(fd);
				CGD_ASSERT(has);

				while (token.type == TKN_ADDR) {
					Addr calledAddr;
#if ENABLE_PROFILING
					ULong count = 0;
#endif

					calledAddr = token.data.addr;
					has = next_token(fd);
					CGD_ASSERT(has);

					if (token.type == TKN_COLON) {
						has = next_token(fd);
						CGD_ASSERT(has && token.type == TKN_NUMBER);

#if ENABLE_PROFILING
						if (!CGD_(clo).ignore_profiling)
							count = token.data.number;
#endif

						has = next_token(fd);
						CGD_ASSERT(has);
					}

#if ENABLE_PROFILING
					add_call2node(cfg, node, CGD_(get_cfg)(calledAddr), count);
#else
					add_call2node(cfg, node, CGD_(get_cfg)(calledAddr));
#endif
				}

				CGD_ASSERT(has && token.type == TKN_BRACKET_CLOSE);

				has = next_token(fd);
				CGD_ASSERT(has && token.type == TKN_BRACKET_OPEN);

				has = next_token(fd);
				CGD_ASSERT(has);

				while (token.type == TKN_NUMBER) {
					Int signum;
					Addr calledAddr;
#if ENABLE_PROFILING
					ULong count = 0;
#endif

					signum = token.data.number;

					has = next_token(fd);
					CGD_ASSERT(has && token.type == TKN_ARROW);

					has = next_token(fd);
					CGD_ASSERT(has && token.type == TKN_ADDR);
					calledAddr = token.data.addr;

					has = next_token(fd);
					CGD_ASSERT(has);

					if (token.type == TKN_COLON) {
						has = next_token(fd);
						CGD_ASSERT(has && token.type == TKN_NUMBER);

#if ENABLE_PROFILING
						if (!CGD_(clo).ignore_profiling)
							count = token.data.number;
#endif

						has = next_token(fd);
						CGD_ASSERT(has);
					}

#if ENABLE_PROFILING
					add_sighandler2node(cfg, node, CGD_(get_cfg)(calledAddr), signum, count);
#else
					add_sighandler2node(cfg, node, CGD_(get_cfg)(calledAddr), signum);
#endif
				}

				CGD_ASSERT(has && token.type == TKN_BRACKET_CLOSE);

				has = next_token(fd);
				CGD_ASSERT(has && token.type == TKN_BOOL);

				if (token.data.bool)
					mark_indirect(cfg, node);

				has = next_token(fd);
				CGD_ASSERT(has && token.type == TKN_BRACKET_OPEN);

				has = next_token(fd);
				CGD_ASSERT(has);

				while (token.type == TKN_EXIT ||
					   token.type == TKN_HALT ||
					   token.type == TKN_ADDR) {
					CfgNode* dst = 0;
#if ENABLE_PROFILING
					ULong count = 0;
#endif

					switch (token.type) {
						case TKN_EXIT:
							dst = cfgnode_exit(cfg);

							has = next_token(fd);
							CGD_ASSERT(has);
							break;
						case TKN_HALT:
							dst = cfgnode_halt(cfg);

							has = next_token(fd);
							CGD_ASSERT(has);
							break;
						case TKN_ADDR:
							ref = cfg_instr_find(cfg, token.data.addr);
							if (!ref) {
								ref = new_instr_ref(CGD_(get_instr)(token.data.addr, 0));
								new_cfgnode_phantom(cfg, ref);
							}

							dst = ref->node;

							has = next_token(fd);
							CGD_ASSERT(has);

							break;
						default:
							tl_assert(0);
					}

					if (token.type == TKN_COLON) {
						has = next_token(fd);
						CGD_ASSERT(has && token.type == TKN_NUMBER);

#if ENABLE_PROFILING
						if (!CGD_(clo).ignore_profiling)
							count = token.data.number;
#endif

						has = next_token(fd);
						CGD_ASSERT(has);
					}

#if ENABLE_PROFILING
					add_edge2nodes(cfg, node, dst, count);
#else
					add_edge2nodes(cfg, node, dst);
#endif
				}

				CGD_ASSERT(has && token.type == TKN_BRACKET_CLOSE);

				break;
			default:
				tl_assert(0);
		}

		has = next_token(fd);
		CGD_ASSERT(has && token.type == TKN_BRACKET_CLOSE);
	}

	// Check the CFG's
	CGD_(forall_cfg)(CGD_(check_cfg));
}

static
Bool cmp_strings(HChar* str1, HChar* str2) {
	return str1 != 0 && str2 != 0 && VG_(strcasecmp)(str1, str2) == 0;
}

void CGD_(dump_cfg)(CFG* cfg) {
	HChar* funct;

	CGD_ASSERT(cfg != 0);

	funct = cfg->fdesc ? CGD_(fdesc_function_name)(cfg->fdesc) : 0;
	if (CGD_(clo).dump_cfgs.all ||
		(CGD_(clo).dump_cfgs.addrs != 0 &&
				CGD_(smart_list_contains)(CGD_(clo).dump_cfgs.addrs, (void*) cfg->addr, 0)) ||
		(CGD_(clo).dump_cfgs.fnames != 0 && funct != 0 &&
				CGD_(smart_list_contains)(CGD_(clo).dump_cfgs.fnames, funct,
						(Bool (*)(void*, void*)) cmp_strings))) {
		Int size;
		const HChar* cwd;
		const HChar* dirname;
		HChar* filename;
		VgFile* out;

		cwd = VG_(get_startup_wd)();
		dirname = CGD_(clo).dump_cfgs.dir;
		size = VG_(strlen)(dirname) + 32;

		if (dirname[0] != '/' && cwd) {
			size += VG_(strlen)(cwd);
			filename = (HChar*) CGD_MALLOC("cgd.cfg.dcfg.1", size);
			VG_(snprintf)(filename, size, "%s/%s/cfg-0x%lx.dot",
				cwd, dirname, cfg->addr);
		} else {
			filename = (HChar*) CGD_MALLOC("cgd.cfg.dcfg.1", size);
			VG_(snprintf)(filename, size, "%s/cfg-0x%lx.dot", dirname, cfg->addr);
		}

		out = VG_(fopen)(filename, VKI_O_WRONLY|VKI_O_TRUNC, 0);
		if (out == NULL) {
			out = VG_(fopen)(filename, VKI_O_CREAT|VKI_O_WRONLY,
					VKI_S_IRUSR|VKI_S_IWUSR);
		}
		CGD_ASSERT(out != 0);

		CGD_(fprint_detailed_cfg)(out, cfg);

		VG_(fclose)(out);

		VG_(free)(filename);
	}
}

void CGD_(forall_cfg)(void (*func)(CFG*)) {
	UInt i;
	CFG *cfg, *tmp;

	for (i = 0; i < cfgs.size; i++) {
		cfg = cfgs.table[i];
		while (cfg) {
			tmp = cfg->chain;
			(*func)(cfg);
			cfg = tmp;
		}
	}
}

void CGD_(clear_visited)(CFG* cfg) {
	CGD_ASSERT(cfg != 0);

	cfg->visited = False;
}

#if ENABLE_PROFILING && CFG_NODE_CACHE_SIZE > 0
void CGD_(cfgnode_flush_edge_count)(CFG* cfg, CfgNode* working, CfgNodeBlockCache* cache) {
	UInt size;
	CfgEdge* edge;

	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(working != 0);
	CGD_ASSERT(cache != 0);

	size = 0;
	while (size < cache->size) {
		edge = get_succ_edge(cfg, working, (cache->addr + size));
		CGD_ASSERT(edge != 0);

		edge->count += cache->count;
		size += CGD_(cfgnode_size)(edge->dst);

		working = edge->dst;
	}
	CGD_ASSERT(size == cache->size);
	CGD_ASSERT(CGD_(cfgnodes_cmp)(working, cache->working));

	// Mark the CFG as dirty.
	cfg->dirty = True;
}

void CGD_(cfgnode_flush_call_count)(CFG* cfg, CfgNode* working, CfgNodeCallCache* cache) {
	CfgCall* cfgCall;

	CGD_UNUSED(cfg);
	CGD_ASSERT(working != 0);
	CGD_ASSERT(cache != 0);

	cfgCall = find_call(working, cache->called);
	CGD_ASSERT(cfgCall != 0);

	cfgCall->count += cache->count;

	// Mark the CFG as dirty.
	cfg->dirty = True;
}

void CGD_(cfg_flush_all_counts)(CFG* cfg) {
	Int i, j, size;

	size = CGD_(smart_list_count)(cfg->nodes);
	for (i = 0; i < size; i++) {
		CfgNode* node;

		node = (CfgNode*) CGD_(smart_list_at)(cfg->nodes, i);
		CGD_ASSERT(node != 0);

		if (node->cache.block) {
			for (j = 0; j < CFG_NODE_CACHE_SIZE; j++) {
				CfgNodeBlockCache* blockCache = &(node->cache.block[j]);
				if (blockCache->count > 0)
					CGD_(cfgnode_flush_edge_count)(cfg, node, blockCache);
			}
		}

		if (node->cache.call) {
			for (j = 0; j < CFG_NODE_CACHE_SIZE; j++) {
				CfgNodeCallCache* callCache = &(node->cache.call[j]);
				if (callCache->count > 0)
					CGD_(cfgnode_flush_call_count)(cfg, node, callCache);
			}
		}

		if (node->cache.exit.enabled && node->cache.exit.count > 0) {
			CfgEdge* edge = find_edge(node, cfg->exit);
			CGD_ASSERT(edge != 0);

			edge->count += node->cache.exit.count;
		}
	}
}
#endif
