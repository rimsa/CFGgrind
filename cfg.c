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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "global.h"

struct _CfgInstrRef {
	UniqueInstr* instr;	// The instruction itself.
	CfgNode* node;		// Reference to the CFG node.

	CfgInstrRef* next;	// Next instruction in block. Nil if last.
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
	SmartList* calls;	// SmartList<CFG*>

	Bool indirect;				/* has an indirect call or jump */
};

/* CFG hash, resizable */
cfg_hash cfgs;

VgFile *fp = 0;

struct {
	enum {
		TKN_BRACKET_OPEN,
		TKN_BRACKET_CLOSE,
		TKN_CFG,
		TKN_NODE,
		TKN_EXIT,
		TKN_HALT,
		TKN_ADDR,
		TKN_NUMBER,
		TKN_BOOL,
		TKN_TEXT,
	} type;

	union {
		Addr addr;
		Int number;
		Bool bool;
	} data;

	HChar text[1024];
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
Bool has_nodes_edge(CfgNode* from, CfgNode* to) {
	Int i, count;

	CGD_ASSERT(from != 0 && (from->type != CFG_EXIT && from->type != CFG_HALT));
	CGD_ASSERT(to != 0 && to->type != CFG_ENTRY);

	CGD_ASSERT(from->info.successors != 0);
	count = CGD_(smart_list_count)(from->info.successors);
	for (i = 0; i < count; i++) {
		CfgNode* tmp = (CfgNode*) CGD_(smart_list_at)(from->info.successors, i);
		CGD_ASSERT(tmp != 0);

		if (CGD_(cfgnodes_cmp)(to, tmp))
			return True;
	}

	// Not found.
	return False;
}

static
Bool add_edge2nodes(CFG* cfg, CfgNode* from, CfgNode* to) {
	if (has_nodes_edge(from, to))
		return False;

	// Add the successor.
	CGD_ASSERT(from->info.successors != 0);
	CGD_(smart_list_add)(from->info.successors, to);

	// Add the predecessor.
	CGD_ASSERT(to->info.predecessors != 0);
	CGD_(smart_list_add)(to->info.predecessors, from);

	// Mark the CFG as dirty.
	cfg->dirty = True;

	return True;
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
		CGD_(smart_list_clear)(block->calls, 0);
		CGD_(delete_smart_list)(block->calls);
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
void cfgnode_add_ref(CFG* cfg, CfgNode* node, CfgInstrRef* ref) {
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

	node = new_cfgnode(CFG_PHANTOM, 1, 1);
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

static __inline__
Bool ref_is_head(CfgInstrRef* ref) {
	CGD_ASSERT(ref && ref->node && ref->node->type == CFG_BLOCK);
	return ref == ref->node->data.block->instrs.leader;
}

static __inline__
Bool ref_is_tail(CfgInstrRef* ref) {
	CGD_ASSERT(ref && ref->node && ref->node->type == CFG_BLOCK);
	return ref == ref->node->data.block->instrs.tail;
}

static
Bool has_node_call(CfgNode* node, CFG* call) {
	Int i, count;

	CGD_ASSERT(node != 0);
	CGD_ASSERT(node->type == CFG_BLOCK);

	if (node->data.block->calls) {
		count = CGD_(smart_list_count)(node->data.block->calls);
		for (i = 0; i < count; i++) {
			CFG* tmp = (CFG*) CGD_(smart_list_at)(node->data.block->calls, i);
			CGD_ASSERT(tmp != 0);

			if (CGD_(cfg_cmp)(call, tmp))
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
Bool cfgnode_add_call(CFG* cfg, CfgNode* node, CFG* call) {
	if (!has_node_call(node, call)) {
		CGD_ASSERT(!CGD_(cfgnode_has_successor_with_addr)(node, call->addr));

		if (!node->data.block->calls)
			node->data.block->calls = CGD_(new_smart_list)(1);

		CGD_(smart_list_add)(node->data.block->calls, call);

		// Mark the CFG as dirty.
		cfg->dirty = True;

		return True;
	}

	return False;
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

	// Move the predecessors of node to pred.
	CGD_(smart_list_copy)(pred->info.predecessors, node->info.predecessors);
	CGD_(smart_list_clear)(node->info.predecessors, 0);

	// Fix the predecessor's successors.
	CGD_ASSERT(pred->info.predecessors != 0);
	size = CGD_(smart_list_count)(pred->info.predecessors);
	for (i = 0; i < size; i++) {
		Int j, size2;
		CfgNode* tmp;

		tmp = (CfgNode*) CGD_(smart_list_at)(pred->info.predecessors, i);
		CGD_ASSERT(tmp != 0);

		CGD_ASSERT(tmp->info.successors != 0);
		size2 = CGD_(smart_list_count)(tmp->info.successors);
		for (j = 0; j < size2; j++) {
			CfgNode* tmp2 = (CfgNode*) CGD_(smart_list_at)(tmp->info.successors, j);
			CGD_ASSERT(tmp2 != 0);

			// Update the node and finish the search.
			if (CGD_(cfgnodes_cmp)(tmp2, node)) {
				CGD_(smart_list_set)(tmp->info.successors, j, pred);
				break;
			}
		}

		// Did we find it?
		CGD_ASSERT(j < size2);
	}

	// Finally, connect both nodes.
	add_edge2nodes(cfg, pred, node);

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

	cfg->entry = new_cfgnode(CFG_ENTRY, 1, 0);
	CGD_(smart_list_add)(cfg->nodes, cfg->entry);

	cfg->cache.refs = CGD_(new_smart_hash)(137);

	CGD_(stat).distinct_cfgs++;

	return cfg;
}

static
void delete_cfg(CFG* cfg) {
	Int i, size;

	CGD_ASSERT(cfg != 0);

	if (cfg->fdesc)
		CGD_(delete_fdesc)(cfg->fdesc);

	size = CGD_(smart_list_count)(cfg->nodes);
	for (i = 0; i < size; i++) {
		CfgNode* node = (CfgNode*) CGD_(smart_list_at)(cfg->nodes, i);
		CGD_ASSERT(node != 0);

		delete_cfgnode(node);

		CGD_(smart_list_set)(cfg->nodes, i, 0);
	}

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
	if (CGD_(is_main_function)(cfg->fdesc))
		cfg->inside_main = True;
}

Bool CGD_(cfg_is_inside_main)(CFG* cfg) {
	return cfg ? cfg->inside_main : False;
}

static
void mark_inside_main(CFG* cfg) {
	Int i, size;

	CGD_ASSERT(cfg != 0);

	// Ignore visited CFG's.
	if (cfg->visited)
		return;

	cfg->inside_main = True;
	cfg->visited = True;

	size = CGD_(smart_list_count)(cfg->nodes);
	for (i = 0; i < size; i++) {
		CfgNode* tmp = (CfgNode*) CGD_(smart_list_at)(cfg->nodes, i);
		CGD_ASSERT(tmp != 0);

		if (tmp->type == CFG_BLOCK) {
			// If there are calls in it, mark them.
			if (tmp->data.block->calls) {
				Int j, size2;

				size2 = CGD_(smart_list_count)(tmp->data.block->calls);
				for (j = 0; j < size2; j++) {
					CFG* called_cfg = (CFG*) CGD_(smart_list_at)(tmp->data.block->calls, j);
					CGD_ASSERT(called_cfg != 0);

					mark_inside_main(called_cfg);
				}
			}
		}
	}
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

void CGD_(cfg_set_inside_main)(CFG* cfg, Bool inside_main) {
	CGD_ASSERT(cfg != 0);

	// Ignore if we already marked this CFG inside main.
	if (cfg->inside_main) {
		CGD_ASSERT(inside_main);
		return;
	}

	// Ignore if we are not changing the status of the CFG.
	if (!inside_main)
		return;

	// All the nested CFG's called from this must also be inside main.
	CGD_(forall_cfg)(CGD_(clear_visited), True);
	mark_inside_main(cfg);
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

Addr CGD_(cfgnode_size)(CfgNode* node) {
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

Bool CGD_(cfgnode_has_call_with_addr)(CfgNode* node, Addr addr) {
	Int i, size;

	CGD_ASSERT(node != 0 && node->type == CFG_BLOCK);
	CGD_ASSERT(addr != 0);

	if (node->data.block->calls) {
		size = CGD_(smart_list_count)(node->data.block->calls);
		for (i = 0; i < size; i++) {
			CFG* cfg = (CFG*) CGD_(smart_list_at)(node->data.block->calls, i);
			CGD_ASSERT(cfg != 0);

			if (cfg->addr == addr)
				return True;
		}
	}

	return False;
}

Bool CGD_(cfgnode_has_successor_with_addr)(CfgNode* node, Addr addr) {
	Int i, size;

	CGD_ASSERT(node != 0);
	CGD_ASSERT(addr != 0);

	size = CGD_(smart_list_count)(node->info.successors);
	for (i = 0; i < size; i++) {
		CfgNode* succ = (CfgNode*) CGD_(smart_list_at)(node->info.successors, i);
		CGD_ASSERT(succ != 0);

		// Ignore exit nodes. Entry nodes should never be successor.
		if (succ->type == CFG_EXIT || succ->type == CFG_HALT)
			continue;

		if (CGD_(cfgnode_addr)(succ) == addr)
			return True;
	}

	return False;
}

void CGD_(cfgnode_remove_successor_with_addr)(CFG* cfg, CfgNode* node, Addr addr) {
	Int i, size;

	if (!node->info.successors)
		return;

	size = CGD_(smart_list_count)(node->info.successors);
	for (i = 0; i < size; i++) {
		CfgNode* succ = (CfgNode*) CGD_(smart_list_at)(node->info.successors, i);
		CGD_ASSERT(succ != 0);

		// Ignore exit nodes.
		// Entry nodes should never be sucessor.
		if (succ->type == CFG_EXIT || succ->type == CFG_HALT)
			continue;

		// Check if we have a successor with this address.
		// If that is the case, remove it if possible.
		if (CGD_(cfgnode_addr)(succ) == addr) {
			Int j, size2;
			CfgNode* last;

			switch (succ->type) {
				case CFG_BLOCK:
					CGD_ASSERT(succ->info.predecessors != 0);

					// Ensure that it has more than one predecessor, the node
					// will not be orphan.
					size2 = CGD_(smart_list_count)(succ->info.predecessors);
					CGD_ASSERT(size2 > 1);

					for (j = 0; j < size2; j++) {
						CfgNode* pred = (CfgNode*) CGD_(smart_list_at)(succ->info.predecessors, j);
						CGD_ASSERT(pred != 0);

						if (pred == node)
							break;
					}

					// Did we find it?
					CGD_ASSERT(j < size2);

					// Remove predecessor.
					last = CGD_(smart_list_at)(succ->info.predecessors, (size2 - 1));
					CGD_(smart_list_set)(succ->info.predecessors, j, last);
					CGD_(smart_list_set)(succ->info.predecessors, (size2 - 1), 0);

					break;
				case CFG_PHANTOM:
					// This means that the node must be a phantom.
					// Ensure that it has only one predecessor (safe to remove).
					CGD_ASSERT(succ->info.predecessors != 0);
					CGD_ASSERT(CGD_(smart_list_count)(succ->info.predecessors) == 1);

					// Remove the phantom node.
					remove_phantom(cfg, succ);

					break;
				default:
					tl_assert(0);
			}

			// Update the sucessors list without it.
			last = (CfgNode*) CGD_(smart_list_at)(node->info.successors, size-1);
			CGD_(smart_list_set)(node->info.successors, i, last);
			CGD_(smart_list_set)(node->info.successors, (size - 1), 0);

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
CfgInstrRef* get_succ_instr(CFG* cfg, CfgNode* from, Addr addr) {
	Int i, size;

	CGD_ASSERT(from->info.successors != 0);
	size = CGD_(smart_list_count)(from->info.successors);
	for (i = 0; i < size; i++) {
		CfgNode* node;
		CfgInstrRef* ref;

		node = (CfgNode*) CGD_(smart_list_at)(from->info.successors, i);
		CGD_ASSERT(node != 0);

		switch (node->type) {
			case CFG_BLOCK:
				ref = node->data.block->instrs.leader;
				CGD_ASSERT(ref != 0);
				break;
			case CFG_PHANTOM:
				ref = node->data.phantom;
				CGD_ASSERT(ref != 0);
				break;
			default:
				ref = 0;
				break;
		}

		// Check if the successors head instruction matches the next address.
		if (ref != 0 && 	ref->instr->addr == addr)
			return ref;
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
#endif

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
			CGD_ASSERT(bb_idx < bb->instr_count);
			addr = base_addr + bb->instr[bb_idx].instr_offset;
			size = bb->instr[bb_idx].instr_size;

			// First check if we have a successor with this address already.
			// Although this conditional block it is not required for the
			// correctness of the algorithm (it is also covered by the next
			// conditional statement), it is considerably faster overall
			// to search the list first and then check the hash if necessary.
			if ((next = get_succ_instr(cfg, working, addr))) {
				// The successor may be a phantom node, in this case convert to a block.
				if (next->node->type == CFG_PHANTOM)
					phantom2block(cfg, next->node, size);

				// Use it as the new working node.
				CGD_ASSERT(ref_is_head(next));
				working = next->node;
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
				add_edge2nodes(cfg, working, next->node);
				working = next->node;
			// In this case, the instruction is new and we can:
			// (1) append it to the working node (if possible); or
			// (2) create a new block with it as the head instruction that
			// will be connected to the working. This block will be the
			// new working node.
			} else {
				next = new_instr_ref(CGD_(get_instr)(addr, size));
				// Append the instruction if possible.
				if (bb_idx > 0 && working->type == CFG_BLOCK &&
					!cfgnode_has_successors(working) && !cfgnode_has_calls(working)) {
					CGD_ASSERT((working->data.block->instrs.tail->instr->addr +
							working->data.block->instrs.tail->instr->size) == addr);
					cfgnode_add_ref(cfg, working, next);
				// Create a new block, connect the working to it and
				// make it the new working node.
				} else {
					CfgNode* node = new_cfgnode_block(cfg, next);
					add_edge2nodes(cfg, working, node);
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

	// If we didn't reach the end of the block, we must split it.
	if (curr && !ref_is_tail(curr))
		working = cfgnode_split(cfg, curr->next);

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
	if (CGD_(cfgnode_has_call_with_addr)(working, to))
		return;

	// Get the successor if it has the leader with address "to"
	next = get_succ_instr(cfg, working, to);

	// If it does not exist, take some actions.
	if (!next) {
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
		add_edge2nodes(cfg, working, next->node);
	}
}

void CGD_(cfgnode_set_call)(CFG* cfg, CfgNode* working, CFG* call, Bool indirect) {
#if CFG_NODE_CACHE_SIZE > 0
	CfgNodeCallCache* cache;
#endif

	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(working != 0);
	CGD_ASSERT(working->type == CFG_BLOCK);
	CGD_ASSERT(call != 0);

#if CFG_NODE_CACHE_SIZE > 0
	cache = cfgcall_cache(working, call->addr);
	cache->addr = call->addr;
	cache->indirect = indirect;
#endif

	if (indirect)
		mark_indirect(cfg, working);

	if (cfgnode_add_call(cfg, working, call)) {
		// If we are adding a call to a CFG that is inside main,
		// mark the called CFG as inside main as well.
		if (cfg->inside_main)
			CGD_(cfg_set_inside_main)(call, True);
	}
}

CfgNode* CGD_(cfgnode_set_exit)(CFG* cfg, CfgNode* working) {
	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(working != 0);
	CGD_ASSERT(working->type == CFG_BLOCK);

#if CFG_NODE_CACHE_SIZE > 0
	working->cache.exit = True;
#endif

	// Add the node if it is does not exist yet.
	add_edge2nodes(cfg, working, cfgnode_exit(cfg));
	return cfg->exit;
}

CfgNode* CGD_(cfgnode_set_halt)(CFG* cfg, CfgNode* working) {
	CGD_ASSERT(cfg != 0);
	CGD_ASSERT(working != 0);
	CGD_ASSERT(working->type == CFG_BLOCK);

	// Add the node if it is does not exist yet.
	add_edge2nodes(cfg, working, cfgnode_halt(cfg));
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

void CGD_(check_cfg)(CFG* cfg) {
	Int i, j, size, size2;
	Int indirects;

	CGD_ASSERT(cfg != 0);

	CGD_ASSERT(cfg->exit != 0 || cfg->halt != 0);

	indirects = 0;
	size = CGD_(smart_list_count)(cfg->nodes);
	for (i = 0; i < size; i++) {
		CfgNode* node = (CfgNode*) CGD_(smart_list_at)(cfg->nodes, i);
		CGD_ASSERT(node != 0);

		switch (node->type) {
			case CFG_ENTRY:
				{
					CfgNode* tmp;
					CfgInstrRef* ref;

					// An entry node has no predecessors and only a single successor.
					CGD_ASSERT(node->info.predecessors == 0);
					CGD_ASSERT(node->info.successors != 0 && CGD_(smart_list_count)(node->info.successors) == 1);

					tmp = (CfgNode*) CGD_(smart_list_at)(node->info.successors, 0);
					CGD_ASSERT(tmp != 0 && tmp->type == CFG_BLOCK);

					// We must have at least one instruction per block.
					CGD_ASSERT(tmp->data.block->instrs.count > 0);

					// The first instruction address must match the cfg address.
					ref = tmp->data.block->instrs.leader;
					CGD_ASSERT(ref != 0);
					CGD_ASSERT(cfg->addr == ref->instr->addr);
				}

				break;
			case CFG_EXIT:
			case CFG_HALT:
				// An exit node has no successors and must have at least one predecessor.
				CGD_ASSERT(node->info.successors == 0);
				CGD_ASSERT(node->info.predecessors != 0 && CGD_(smart_list_count)(node->info.predecessors) > 0);

				break;
			case CFG_BLOCK:
				{
					Int total, count;
					CfgInstrRef* ref;
					CfgBlock* block;

					// A block node must have at least one predecessor and one sucessor.
					CGD_ASSERT(node->info.predecessors != 0 && CGD_(smart_list_count)(node->info.predecessors) > 0);
					CGD_ASSERT(node->info.successors != 0 && CGD_(smart_list_count)(node->info.successors) > 0);

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
							CFG* called = (CFG*) CGD_(smart_list_at)(block->calls, j);
							CGD_ASSERT(called != 0);

							CGD_ASSERT(!CGD_(cfgnode_has_successor_with_addr)(node, called->addr));
						}
					}

					if (block->indirect)
						indirects++;
				}

				break;
			case CFG_PHANTOM:
				// A phantom must have at least one predecessor and no successors.
				CGD_ASSERT(node->info.predecessors != 0 && CGD_(smart_list_count)(node->info.predecessors) > 0);
				CGD_ASSERT(node->info.successors == 0 || CGD_(smart_list_count)(node->info.successors) == 0);

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
					CFG* called_cfg;
					HChar* desc;

					called_cfg = (CFG*) CGD_(smart_list_at)(node->data.block->calls, j);
					CGD_ASSERT(called_cfg != 0);

					VG_(fprintf)(out, "     &nbsp;&nbsp;0x%lx (", called_cfg->addr);

					desc = CGD_(fdesc2str)(called_cfg->fdesc);
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

		if (node->info.successors != 0) {
			size2 = CGD_(smart_list_count)(node->info.successors);
			for (j = 0; j < size2; j++) {
				CfgNode* succ = (CfgNode*) CGD_(smart_list_at)(node->info.successors, j);
				CGD_ASSERT(succ != 0);
				CGD_ASSERT(succ->type != CFG_ENTRY);

				// If it has successors, it shouldn't be an exit.
				CGD_ASSERT(node->type != CFG_EXIT && node->type != CFG_HALT);

				if (node->type == CFG_ENTRY)
					VG_(fprintf)(out, "  %s -> ", CGD_(cfgnode_type2str)(node->type, False));
				else
					VG_(fprintf)(out, "  \"0x%lx\" -> ", CGD_(cfgnode_addr)(node));

				if (succ->type == CFG_EXIT || succ->type == CFG_HALT)
					VG_(fprintf)(out, "%s\n", CGD_(cfgnode_type2str)(succ->type, False));
				else
					VG_(fprintf)(out, "\"0x%lx\"", CGD_(cfgnode_addr)(succ));

				// Dashed edge if successor ir phantom.
				if (succ->type == CFG_PHANTOM)
					VG_(fprintf)(out, " [style=dashed]");

				VG_(fprintf)(out, "\n");
			}
		}

		if (CGD_(cfgnode_is_indirect)(node)) {
			VG_(fprintf)(out, "  \"Unknown%d\" [label=\"?\", shape=none]\n", unknown);
			VG_(fprintf)(out, "  \"0x%lx\" -> \"Unknown%d\" [style=dashed]\n",
					CGD_(cfgnode_addr)(node), unknown);
			unknown++;
		}
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

	VG_(fprintf)(fp, "[cfg 0x%lx %s \"", cfg->addr, (cfg->inside_main ? "true" : "false"));
	if (!cfg->fdesc)
		CGD_(cfg_build_fdesc)(cfg);

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

		VG_(fprintf)(fp, "[node 0x%lx ", cfg->addr);

		if (node->type == CFG_BLOCK) {
			VG_(fprintf)(fp, "0x%lx ", node->data.block->addr);

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
					CFG* called = (CFG*) CGD_(smart_list_at)(node->data.block->calls, j);
					CGD_ASSERT(called != 0);

					if (j > 0)
						VG_(fprintf)(fp, " ");

					VG_(fprintf)(fp, "0x%lx", called->addr);
				}
			}
			VG_(fprintf)(fp, "] ");

			VG_(fprintf)(fp, "%s ", node->data.block->indirect ? "true" : "false");

			VG_(fprintf)(fp, "[");
			size2 = CGD_(smart_list_count)(node->info.successors);
			for (j = 0; j < size2; j++) {
				CfgNode* succ = (CfgNode*) CGD_(smart_list_at)(node->info.successors, j);
				CGD_ASSERT(succ != 0);

				if (j > 0)
					VG_(fprintf)(fp, " ");

				switch (succ->type) {
					case CFG_EXIT:
					case CFG_HALT:
						VG_(fprintf)(fp, "%s", CGD_(cfgnode_type2str)(succ->type, True));
						break;
					case CFG_BLOCK:
					case CFG_PHANTOM:
						VG_(fprintf)(fp, "0x%lx", CGD_(cfgnode_addr)(succ));
						break;
					default:
						tl_assert(0);
				}
			}
			VG_(fprintf)(fp, "]");
		} else {
			tl_assert(0);
		}

		VG_(fprintf)(fp, "]\n");
	}
}

void CGD_(write_cfgs)(VgFile *out_fp) {
	CGD_ASSERT(out_fp != 0);

	fp = out_fp;
	CGD_(forall_cfg)(write_cfg, True);
	fp = 0;
}

static
Bool next_token(Int fd) {
    Int idx, state;
    static Int last = -1;

    idx = 0;
    VG_(memset)(&token, 0, sizeof(token));

    state = 1;
    while (state != 8) {
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
					state = 8;
				} else if (c == ']') {
					token.text[idx++] = c;
					token.type = TKN_BRACKET_CLOSE;
					state = 8;
				} else if (c == '\"') {
					token.type = TKN_TEXT;
					state = 6;
				} else if (c == '#') {
					state = 7;
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

					state = 8;
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

					state = 8;
				}

            		break;
            case 4:
				if (c >= '0' && c <= '9') {
					token.text[idx++] = c;
					state = 4;
				} else {
					token.data.number = VG_(strtoll10)(token.text, 0);

					if (c != -1)
						last = c;

					state = 8;
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

					state = 8;
				}

				break;
			case 6:
				if (c != -1) {
					if (c == '\"')
						state = 8;
					else {
						token.text[idx++] = c;
						state = 6;
					}
				} else {
					tl_assert(0);
				}

				break;
			case 7:
				if (c == -1) {
					return False;
				} else {
					if (c == '\n')
						state = 1;
					else
						state = 7;
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
	Int size;

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
				CGD_ASSERT(has && token.type == TKN_BOOL);
				cfg->inside_main = token.data.bool;

				has = next_token(fd);
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
				CGD_ASSERT(has && token.type == TKN_BRACKET_OPEN);

				has = next_token(fd);
				CGD_ASSERT(has && token.type == TKN_NUMBER);
				size = token.data.number;

				// If the reference exists, then it must be a phantom
				// node and we will convert it to a block node.
				if (ref) {
					CGD_ASSERT(ref->node->type == CFG_PHANTOM);
					node = ref->node;
					phantom2block(cfg, node, size);
				// Otherwise, we will create the block node.
				} else {
					ref = new_instr_ref(CGD_(get_instr)(addr, size));
					node = new_cfgnode_block(cfg, ref);
				}
				CGD_ASSERT(node->type == CFG_BLOCK);

				// If the address match the CFG's addr, then it is the entry block.
				if (addr == cfg->addr)
					add_edge2nodes(cfg, cfg->entry, node);

				addr += size;

				has = next_token(fd);
				CGD_ASSERT(has);

				while (token.type == TKN_NUMBER) {
					size = token.data.number;

					cfgnode_add_ref(cfg, node,
							new_instr_ref(CGD_(get_instr)(addr, size)));

					addr += size;

					has = next_token(fd);
					CGD_ASSERT(has);
				}

				CGD_ASSERT(has && token.type == TKN_BRACKET_CLOSE);

				has = next_token(fd);
				CGD_ASSERT(has && token.type == TKN_BRACKET_OPEN);

				has = next_token(fd);
				CGD_ASSERT(has);

				while (token.type == TKN_ADDR) {
					if (node->data.block->calls == 0)
						node->data.block->calls = CGD_(new_smart_list)(1);

					CGD_(smart_list_add)(node->data.block->calls, CGD_(get_cfg)(token.data.addr));

					has = next_token(fd);
					CGD_ASSERT(has);
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
					switch (token.type) {
						case TKN_EXIT:
							add_edge2nodes(cfg, node, cfgnode_exit(cfg));

							has = next_token(fd);
							CGD_ASSERT(has);
							break;
						case TKN_HALT:
							add_edge2nodes(cfg, node, cfgnode_halt(cfg));

							has = next_token(fd);
							CGD_ASSERT(has);
							break;
						case TKN_ADDR:
							ref = cfg_instr_find(cfg, token.data.addr);
							if (!ref) {
								ref = new_instr_ref(CGD_(get_instr)(token.data.addr, 0));
								new_cfgnode_phantom(cfg, ref);
							}

							add_edge2nodes(cfg, node, ref->node);

							has = next_token(fd);
							CGD_ASSERT(has);

							break;
						default:
							tl_assert(0);
					}
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
	CGD_(forall_cfg)(CGD_(check_cfg), True);
}

static
Bool cmp_strings(HChar* str1, HChar* str2) {
	return str1 != 0 && str2 != 0 && VG_(strcmp)(str1, str2) == 0;
}

void CGD_(dump_cfg)(CFG* cfg) {
	VgFile* out;
	HChar filename[256];
	HChar* funct;

	CGD_ASSERT(cfg != 0);

	funct = cfg->fdesc ? CGD_(fdesc_function_name)(cfg->fdesc) : 0;
	if (CGD_(clo).dump_cfgs.all ||
		(CGD_(clo).dump_cfgs.addrs != 0 &&
				CGD_(smart_list_contains)(CGD_(clo).dump_cfgs.addrs, (void*) cfg->addr, 0)) ||
		(CGD_(clo).dump_cfgs.fnames != 0 && funct != 0 &&
				CGD_(smart_list_contains)(CGD_(clo).dump_cfgs.fnames, funct,
						(Bool (*)(void*, void*)) cmp_strings))) {
		VG_(snprintf)(filename, sizeof(filename)/sizeof(HChar),
				"%s/cfg-0x%lx.dot", CGD_(clo).dump_cfgs.dir, cfg->addr);

		out = VG_(fopen)(filename, VKI_O_WRONLY|VKI_O_TRUNC, 0);
		if (out == NULL) {
			out = VG_(fopen)(filename, VKI_O_CREAT|VKI_O_WRONLY,
					VKI_S_IRUSR|VKI_S_IWUSR);
		}
		CGD_ASSERT(out != 0);

		CGD_(fprint_detailed_cfg)(out, cfg);

		VG_(fclose)(out);
	}
}

void CGD_(forall_cfg)(void (*func)(CFG*), Bool all) {
	UInt i;
	CFG *cfg, *tmp;

	for (i = 0; i < cfgs.size; i++) {
		cfg = cfgs.table[i];
		while (cfg) {
			tmp = cfg->chain;

			if (all || cfg->inside_main)
				(*func)(cfg);

			cfg = tmp;
		}
	}
}

void CGD_(clear_visited)(CFG* cfg) {
	CGD_ASSERT(cfg != 0);

	cfg->visited = False;
}
