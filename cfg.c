#include "global.h"

struct _CFG {
	Addr addr;				// CFG address
	FunctionDesc* fdesc;		// debugging info for this CFG

	Bool inside_main;		// mark the CFG as a successor of the main func
	Bool dirty;				// true if new nodes are added during analysis
	Bool visited;			// used to use in search algorithms

	CfgNode* entry;			// cfg entry node
	CfgNode* exit;			// cfg exit node (if exists).
	CfgNode* halt;			// cfg halt node (if exists).
	SmartList* nodes;		// SmartList<CfgNode*>

	struct {
		SmartHash* refs;		// SmartHash<CfgInstrRef*>, index by instruction address
	} cache;

	struct {
		Int blocks;
		Int phantoms;
		Int indirects;
	} stats;

	CFG* chain;				// entry chain in hash
};

typedef struct _CfgInstrRef	CfgInstrRef;
struct _CfgInstrRef {
	UniqueInstr* instr;	// The instruction itself.
	CfgNode* node;		// Reference to the CFG node.

	CfgInstrRef* next;	// Next instruction in block. Nil if last.
};

typedef struct _CfgBlock CfgBlock;
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

struct _CfgNode {
	Int id;
	enum CfgNodeType type;

	union {
		CfgInstrRef* phantom;	/* Phantom instruction */
		CfgBlock* block;			/* Block node's block */
	} data;

	struct {
		struct {
			SmartList* nodes;	/* SmartList<CfgNode*> */
			BitSet* flags;		/* For each node, if it is virtual (set bit) or not (clear bit) */
		} successors, predecessors;

		SmartList* dominators;
		CfgNode* idom; 			// imediate dominator
	} info;

	Bool visited;				// mark of visited node
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
		TKN_PRIME
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
	LPG_ASSERT(ref != 0 && ref->instr != 0);
	return ref->instr->addr;
}

static __inline__
CfgInstrRef* new_instr_ref(UniqueInstr* instr) {
	CfgInstrRef* ref;

	LPG_ASSERT(instr != 0);

	ref = (CfgInstrRef*) LPG_MALLOC("lg.cfg.nil.1", sizeof(CfgInstrRef));
	VG_(memset)(ref, 0, sizeof(CfgInstrRef));

	ref->instr = instr;

	return ref;
}

static __inline__
void delete_instr_ref(CfgInstrRef* ref) {
	LPG_ASSERT(ref != 0);
	LPG_DATA_FREE(ref, sizeof(CfgInstrRef));
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
    new_table = (CFG**) LPG_MALLOC("cl.cfg.rct.1",
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

    LPG_FREE(cfgs.table);

    LPG_DEBUG(0, "Resize CFG Hash: %u => %d (entries %u, conflicts %d)\n",
	     cfgs.size, new_size,
	     cfgs.entries, conflicts1);

    cfgs.size  = new_size;
    cfgs.table = new_table;
    LPG_(stat).cfg_hash_resizes++;
}

static
CFG* lookup_cfg(Addr addr) {
	CFG* cfg;
	Int idx;

	LPG_ASSERT(addr != 0);

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

	LPG_ASSERT(cfg != 0);
	LPG_ASSERT(node != 0);

	count = LPG_(smart_list_count)(cfg->nodes);
	for (i = 0; i < count; i++) {
		CfgNode* tmp = (CfgNode*) LPG_(smart_list_at)(cfg->nodes, i);
		LPG_ASSERT(tmp != 0);

		if (LPG_(cfgnodes_cmp)(node, tmp))
			return True;
	}

	// Not found.
	return False;
}

static
Bool add_node2cfg(CFG* cfg, CfgNode* node) {
	if (!has_cfg_node(cfg, node)) {
		// We can only add block or phantom nodes.
		LPG_ASSERT(node->type == CFG_BLOCK ||
				   node->type == CFG_PHANTOM);

		// Add the node to the list of nodes in the CFG.
		LPG_(smart_list_add)(cfg->nodes, node);

		// Mark the CFG as dirty.
		cfg->dirty = True;

		return True;
	}

	return False;
}

static
Bool has_nodes_edge(CfgNode* from, CfgNode* to, Bool* is_virtual) {
	Int i, count;

	LPG_ASSERT(from != 0 && (from->type != CFG_EXIT && from->type != CFG_HALT));
	LPG_ASSERT(to != 0 && to->type != CFG_ENTRY);

	LPG_ASSERT(from->info.successors.nodes != 0);
	count = LPG_(smart_list_count)(from->info.successors.nodes);
	for (i = 0; i < count; i++) {
		CfgNode* tmp = (CfgNode*) LPG_(smart_list_at)(from->info.successors.nodes, i);
		LPG_ASSERT(tmp != 0);

		if (LPG_(cfgnodes_cmp)(to, tmp)) {
			if (is_virtual) {
				LPG_ASSERT(from->info.successors.flags != 0);
				*is_virtual = LPG_(bitset_get_pos)(from->info.successors.flags, i);
			}

			return True;
		}
	}

	// Not found.
	return False;
}

static
Bool add_edge2nodes(CFG* cfg, CfgNode* from, CfgNode* to, Bool virtual) {
	Int i, size;
	Bool has, old_virtual;

	has = has_nodes_edge(from, to, &old_virtual);
	if (has && old_virtual == virtual)
		return False;

	if (!has) {
		// Add the successor.
		LPG_ASSERT(from->info.successors.nodes != 0);
		LPG_(smart_list_add)(from->info.successors.nodes, to);

		// Add the predecessor.
		LPG_ASSERT(to->info.predecessors.nodes != 0);
		LPG_(smart_list_add)(to->info.predecessors.nodes, from);

		// If it is not virtual, we don't need to update the flags.
		if (!virtual)
			goto out;
	} else {
		// If the edge already exists (it may be virtual or not),
		// then it does not matter if we are requesting a virtual edge.
		if (virtual)
			goto out;
	}

	LPG_ASSERT(from->info.successors.flags != 0);
	size = LPG_(smart_list_count)(from->info.successors.nodes);
	for (i = 0; i < size; i++) {
		CfgNode* tmp = (CfgNode*) LPG_(smart_list_at)(from->info.successors.nodes, i);
		LPG_ASSERT(tmp != 0);

		if (LPG_(cfgnodes_cmp)(tmp, to)) {
			LPG_(bitset_set_pos_value)(from->info.successors.flags, i, virtual);
			break;
		}
	}
	LPG_ASSERT(i < size);

	LPG_ASSERT(to->info.predecessors.flags != 0);
	size = LPG_(smart_list_count)(to->info.predecessors.nodes);
	for (i = 0; i < size; i++) {
		CfgNode* tmp = (CfgNode*) LPG_(smart_list_at)(to->info.predecessors.nodes, i);
		LPG_ASSERT(tmp != 0);

		if (LPG_(cfgnodes_cmp)(tmp, from)) {
			LPG_(bitset_set_pos_value)(to->info.predecessors.flags, i, virtual);
			break;
		}
	}
	LPG_ASSERT(i < size);

out:
	// Mark the CFG as dirty.
	cfg->dirty = True;

	return True;
}

static
CfgNode* new_cfgnode(enum CfgNodeType type, Int succs, Int preds) {
	CfgNode* node;

	LPG_ASSERT(succs >= 0);

	node = (CfgNode*) LPG_MALLOC("cl.cfg.ncn.1", sizeof(CfgNode));
	VG_(memset)(node, 0, sizeof(CfgNode));

	node->id = ++LPG_(stat).distinct_cfg_nodes;
	node->type = type;

	if (succs > 0) {
		node->info.successors.nodes = LPG_(new_smart_list)(succs);
		node->info.successors.flags = LPG_(new_bitset)(succs);
	}

	if (preds > 0) {
		node->info.predecessors.nodes = LPG_(new_smart_list)(preds);
		node->info.predecessors.flags = LPG_(new_bitset)(preds);
	}

	return node;
}

static __inline__
CfgBlock* new_block(CfgInstrRef* ref) {
	CfgBlock* block;

	LPG_ASSERT(ref != 0);
	LPG_ASSERT(ref->instr != 0);

	block = (CfgBlock*) LPG_MALLOC("lg.cfg.nb.1", sizeof(CfgBlock));
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

	LPG_ASSERT(block != 0);

	ref = block->instrs.leader;
	while (ref) {
		next = ref->next;
		delete_instr_ref(ref);
		ref = next;
	}

	if (block->calls) {
		LPG_(smart_list_clear)(block->calls, 0);
		LPG_(delete_smart_list)(block->calls);
	}

	LPG_DATA_FREE(block, sizeof(CfgBlock));
}

static
void cfgnode_put_block(CFG* cfg, CfgNode* node, CfgBlock* block) {
	CfgInstrRef* ref;
	CfgInstrRef* old;

	LPG_ASSERT(cfg != 0);
	LPG_ASSERT(node != 0);
	LPG_ASSERT(block != 0);
	LPG_ASSERT(node->data.block == 0);

	// Add the block to the node.
	node->data.block = block;

	// Get the first instruction in the block.
	ref = block->instrs.leader;
	LPG_ASSERT(ref != 0);
	LPG_ASSERT(node->data.block->addr == ref->instr->addr);

	// Fix the node in all instructions refs and add them to the cache.
	while (ref) {
		// Update the reference node information.
		LPG_ASSERT(ref->node == 0);
		ref->node = node;

		// Add the reference to the CFG cache and ensure it should not be another ref
		// with the same adddress.
		old = LPG_(smart_hash_put)(cfg->cache.refs, ref, (HWord (*)(void*)) ref_instr_addr);
		LPG_ASSERT(old == 0 || old == ref);

		ref = ref->next;
	}

	// Mark the CFG as dirty.
	cfg->dirty = True;
}

static
void cfgnode_put_phantom(CFG* cfg, CfgNode* node, CfgInstrRef* ref) {
	CfgInstrRef* old;

	LPG_ASSERT(cfg != 0);
	LPG_ASSERT(node != 0);
	LPG_ASSERT(ref != 0);
	LPG_ASSERT(node->data.phantom == 0);

	// Add the block to the node.
	node->data.phantom = ref;
	ref->node = node;
	ref->next = 0;

	// Add the reference to the CFG cache and ensure it should not be another ref
	// with the same adddress.
	old = LPG_(smart_hash_put)(cfg->cache.refs, ref, (HWord (*)(void*)) ref_instr_addr);
	LPG_ASSERT(old == 0 || old == ref);

	// Mark the CFG as dirty.
	cfg->dirty = True;
}

static
void cfgnode_add_ref(CFG* cfg, CfgNode* node, CfgInstrRef* ref) {
	CfgInstrRef** last;
	CfgInstrRef* old;

	LPG_ASSERT(cfg != 0);
	LPG_ASSERT(node != 0);
	LPG_ASSERT(node->type == CFG_BLOCK && node->data.block != 0);
	LPG_ASSERT(ref != 0);

	// Get the last instruction reference in the node.
	last = &(node->data.block->instrs.tail);
	LPG_ASSERT(*last != 0 && (*last)->next == 0);
	LPG_ASSERT((*last)->node == node);
	LPG_ASSERT(((*last)->instr->addr + (*last)->instr->size) == ref->instr->addr);

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
	old = LPG_(smart_hash_put)(cfg->cache.refs, ref, (HWord (*)(void*)) ref_instr_addr);
	LPG_ASSERT(old == 0 || old == ref);

	// Mark the CFG as dirty.
	cfg->dirty = True;
}

static
CfgNode* new_cfgnode_block(CFG* cfg, CfgInstrRef* ref) {
	CfgNode* node;

	LPG_ASSERT(cfg != 0);
	LPG_ASSERT(ref != 0);

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

	LPG_ASSERT(cfg != 0);
	LPG_ASSERT(ref != 0);

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
	LPG_ASSERT(cfg != 0);

	if (!cfg->exit) {
		cfg->exit = new_cfgnode(CFG_EXIT, 0, 1);
		LPG_(smart_list_add)(cfg->nodes, cfg->exit);

		// Mark the CFG as dirty.
		cfg->dirty = True;
	}

	return cfg->exit;
}

static
CfgNode* cfgnode_halt(CFG* cfg) {
	LPG_ASSERT(cfg != 0);

	if (!cfg->halt) {
		cfg->halt = new_cfgnode(CFG_HALT, 0, 1);
		LPG_(smart_list_add)(cfg->nodes, cfg->halt);

		// Mark the CFG as dirty.
		cfg->dirty = True;
	}

	return cfg->halt;
}

static
void delete_cfgnode(CfgNode* node) {
	LPG_ASSERT(node != 0);

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

	if (node->info.successors.nodes) {
		LPG_(smart_list_clear)(node->info.successors.nodes, 0);
		LPG_(delete_smart_list)(node->info.successors.nodes);

		LPG_ASSERT(node->info.successors.flags);
		LPG_(delete_bitset)(node->info.successors.flags);
	}

	if (node->info.predecessors.nodes) {
		LPG_(smart_list_clear)(node->info.predecessors.nodes, 0);
		LPG_(delete_smart_list)(node->info.predecessors.nodes);

		LPG_ASSERT(node->info.predecessors.flags);
		LPG_(delete_bitset)(node->info.predecessors.flags);
	}

	if (node->info.dominators) {
		LPG_(smart_list_clear)(node->info.dominators, 0);
		LPG_(delete_smart_list)(node->info.dominators);
	}

	LPG_DATA_FREE(node, sizeof(CfgNode));
}

static __inline__
Bool ref_is_head(CfgInstrRef* ref) {
	LPG_ASSERT(ref && ref->node && ref->node->type == CFG_BLOCK);
	return ref == ref->node->data.block->instrs.leader;
}

static __inline__
Bool ref_is_tail(CfgInstrRef* ref) {
	LPG_ASSERT(ref && ref->node && ref->node->type == CFG_BLOCK);
	return ref == ref->node->data.block->instrs.tail;
}

static
Bool has_node_call(CfgNode* node, CFG* call) {
	Int i, count;

	LPG_ASSERT(node != 0);
	LPG_ASSERT(node->type == CFG_BLOCK);

	if (node->data.block->calls) {
		count = LPG_(smart_list_count)(node->data.block->calls);
		for (i = 0; i < count; i++) {
			CFG* tmp = (CFG*) LPG_(smart_list_at)(node->data.block->calls, i);
			LPG_ASSERT(tmp != 0);

			if (LPG_(cfg_cmp)(call, tmp))
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
	LPG_ASSERT(phantom != 0);
	LPG_ASSERT(phantom->type == CFG_PHANTOM);

	// Free the node's memory.
	pos = LPG_(smart_list_find)(cfg->nodes,
			(Bool (*)(void*, void*)) LPG_(cfgnodes_cmp), phantom);
	LPG_ASSERT(pos != 0 && pos->next == 0);
	LPG_(smart_list_set)(cfg->nodes, pos->index, LPG_(smart_list_tail)(cfg->nodes));
	LPG_(smart_list_set)(cfg->nodes, LPG_(smart_list_count)(cfg->nodes) - 1, 0);
	LPG_(smart_list_delete_value)(pos);

	ref = phantom->data.phantom;
	LPG_ASSERT(ref != 0);

	// Remove the reference from the CFG's instruction cache.
	LPG_(smart_hash_remove)(cfg->cache.refs, ref_instr_addr(ref),
			(HWord (*)(void*)) ref_instr_addr);

	delete_cfgnode(phantom);

	LPG_ASSERT(cfg->stats.phantoms > 0);
	cfg->stats.phantoms--;
}

static
void call_sanity_check(CFG* cfg, CfgNode* node, Addr addr) {
	Int i, size;

	if (!node->info.successors.nodes)
		return;

	size = LPG_(smart_list_count)(node->info.successors.nodes);
	for (i = 0; i < size; i++) {
		CfgNode* succ = (CfgNode*) LPG_(smart_list_at)(node->info.successors.nodes, i);
		LPG_ASSERT(succ != 0);

		// Ignore exit nodes.
		// Entry nodes should never be sucessor.
		if (succ->type == CFG_EXIT || succ->type == CFG_HALT)
			continue;

		// Check if we have a successor with this address.
		// If that is the case, remove it if possible.
		if (LPG_(cfgnode_addr)(succ) == addr) {
			switch (succ->type) {
				case CFG_BLOCK:
					// We can only remove it if the flag is virtual.
					LPG_ASSERT(LPG_(bitset_get_pos)(node->info.successors.flags, i));
					break;
				case CFG_PHANTOM:
					// This means that the node must be a phantom.
					// Ensure that it has only one predecessor (safe to remove).
					LPG_ASSERT(succ->info.predecessors.nodes != 0);
					LPG_ASSERT(LPG_(smart_list_count)(succ->info.predecessors.nodes) == 1);

					// Remove the phantom node.
					remove_phantom(cfg, succ);

					break;
				default:
					tl_assert(0);
			}

			// Update the sucessors list without it.
			CfgNode* last = (CfgNode*) LPG_(smart_list_at)(node->info.successors.nodes, size-1);
			LPG_(smart_list_set)(node->info.successors.nodes, i, last);
			LPG_(smart_list_set)(node->info.successors.nodes, size-1, 0);

			// Update the virtual flag as well.
			LPG_ASSERT(node->info.successors.flags != 0);
			LPG_(bitset_set_pos_value)(node->info.successors.flags, i,
					LPG_(bitset_get_pos)(node->info.successors.flags, size-1));
			LPG_(bitset_clear_pos)(node->info.successors.flags, size-1);

			return;
		}
	}
}

static
Bool cfgnode_add_call(CFG* cfg, CfgNode* node, CFG* call) {
	if (!has_node_call(node, call)) {
		call_sanity_check(cfg, node, call->addr);

		if (!node->data.block->calls)
			node->data.block->calls = LPG_(new_smart_list)(1);

		LPG_(smart_list_add)(node->data.block->calls, call);

		// Mark the CFG as dirty.
		cfg->dirty = True;

		return True;
	}

	return False;
}

static
CfgInstrRef* cfg_instr_find(CFG* cfg, Addr addr) {
	LPG_ASSERT(cfg != 0);
	LPG_ASSERT(addr != 0);

	return (CfgInstrRef*) LPG_(smart_hash_get)(cfg->cache.refs,
				addr, (HWord (*)(void*)) ref_instr_addr);
}

static
CfgNode* cfgnode_split(CFG* cfg, CfgInstrRef* ref) {
	Int i, size;
	CfgNode* node;
	CfgNode* pred;
	CfgInstrRef* first;
	CfgInstrRef* last;

	LPG_ASSERT(cfg != 0);
	LPG_ASSERT(ref != 0);

	node = ref->node;
	LPG_ASSERT(!ref_is_head(ref));

	first = last = node->data.block->instrs.leader;
	while (last->next != ref) {
		// Account for the removal of the current instruction
		// (last at this iteration).
		node->data.block->size -= last->instr->size;
		node->data.block->instrs.count--;

		last = last->next;
		LPG_ASSERT(last != 0);
	}

	// Update the node with the new leader.
	node->data.block->addr = ref->instr->addr;
	node->data.block->size -= last->instr->size;
	node->data.block->instrs.leader = ref;
	node->data.block->instrs.count--;

	// Create a new predecessor node with the first reference until the last.
	last->next = 0;
	pred = new_cfgnode_block(cfg, first);

	// Move the predecessors of node to pred, including the flags.
	LPG_(smart_list_copy)(pred->info.predecessors.nodes, node->info.predecessors.nodes);
	LPG_(smart_list_clear)(node->info.predecessors.nodes, 0);
	LPG_(bitset_copy)(pred->info.predecessors.flags, node->info.predecessors.flags);
	LPG_(bitset_clear)(node->info.predecessors.flags);

	// Fix the predecessor's successors.
	LPG_ASSERT(pred->info.predecessors.nodes != 0);
	size = LPG_(smart_list_count)(pred->info.predecessors.nodes);
	for (i = 0; i < size; i++) {
		Int j, size2;
		CfgNode* tmp;

		tmp = (CfgNode*) LPG_(smart_list_at)(pred->info.predecessors.nodes, i);
		LPG_ASSERT(tmp != 0);

		LPG_ASSERT(tmp->info.successors.nodes != 0);
		size2 = LPG_(smart_list_count)(tmp->info.successors.nodes);
		for (j = 0; j < size2; j++) {
			CfgNode* tmp2 = (CfgNode*) LPG_(smart_list_at)(tmp->info.successors.nodes, j);
			LPG_ASSERT(tmp2 != 0);

			// Update the node and finish the search.
			if (LPG_(cfgnodes_cmp)(tmp2, node)) {
				LPG_(smart_list_set)(tmp->info.successors.nodes, j, pred);
				// Do not update the flags of this predecessor, since it should be mantained the same.
				break;
			}
		}

		// Did we find it?
		LPG_ASSERT(j < size2);
	}

	// Finally, connect both nodes.
	add_edge2nodes(cfg, pred, node, False);

	// Return the created predecessor.
	return pred;
}

static
CFG* new_cfg(Addr addr) {
	CFG* cfg;

	LPG_ASSERT(addr != 0);

	cfg = (CFG*) LPG_MALLOC("cl.cfg.nc.1", sizeof(CFG));
	VG_(memset)(cfg, 0, sizeof(CFG));

	cfg->addr = addr;

	// Create the nodes list.
	cfg->nodes = LPG_(new_smart_list)(3);

	cfg->entry = new_cfgnode(CFG_ENTRY, 1, 0);
	LPG_(smart_list_add)(cfg->nodes, cfg->entry);

	cfg->cache.refs = LPG_(new_smart_hash)(137);

	LPG_(stat).distinct_cfgs++;

	return cfg;
}

static
void delete_cfg(CFG* cfg) {
	Int i, size;

	LPG_ASSERT(cfg != 0);

	if (cfg->fdesc)
		LPG_(delete_fdesc)(cfg->fdesc);

	size = LPG_(smart_list_count)(cfg->nodes);
	for (i = 0; i < size; i++) {
		CfgNode* node = (CfgNode*) LPG_(smart_list_at)(cfg->nodes, i);
		LPG_ASSERT(node != 0);

		delete_cfgnode(node);

		LPG_(smart_list_set)(cfg->nodes, i, 0);
	}

	LPG_(delete_smart_list)(cfg->nodes);

	LPG_(smart_hash_clear)(cfg->cache.refs, 0);
	LPG_(delete_smart_hash)(cfg->cache.refs);

	LPG_DATA_FREE(cfg, sizeof(CFG));
}

void LPG_(init_cfg_hash)() {
	Int size;

	cfgs.size    = 2137;
	cfgs.entries = 0;

	size = cfgs.size * sizeof(CFG*);
	cfgs.table = (CFG**) LPG_MALLOC("cl.cfg.ich.1", size);
	VG_(memset)(cfgs.table, 0, size);
}

void LPG_(destroy_cfg_hash)() {
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

	LPG_ASSERT(cfgs.entries == 0);

	LPG_FREE(cfgs.table);
	cfgs.table = 0;
}

CFG* LPG_(get_cfg)(Addr addr) {
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

Addr LPG_(cfg_addr)(CFG* cfg) {
	LPG_ASSERT(cfg != 0);
	return cfg->addr;
}

FunctionDesc* LPG_(cfg_fdesc)(CFG* cfg) {
	LPG_ASSERT(cfg != 0);
	return cfg->fdesc;
}

void LPG_(cfg_build_fdesc)(CFG* cfg) {
	LPG_ASSERT(cfg != 0);
	LPG_ASSERT(cfg->fdesc == 0);

	// Build the function description and update the cfg if it inside main.
	cfg->fdesc = LPG_(new_fdesc)(cfg->addr, True);
	if (LPG_(is_main_function)(cfg->fdesc))
		cfg->inside_main = True;
}

Bool LPG_(cfg_is_inside_main)(CFG* cfg) {
	return cfg ? cfg->inside_main : False;
}

static
void mark_inside_main(CFG* cfg) {
	Int i, size;

	LPG_ASSERT(cfg != 0);

	// Ignore visited CFG's.
	if (cfg->visited)
		return;

	cfg->inside_main = True;
	cfg->visited = True;

	size = LPG_(smart_list_count)(cfg->nodes);
	for (i = 0; i < size; i++) {
		CfgNode* tmp = (CfgNode*) LPG_(smart_list_at)(cfg->nodes, i);
		LPG_ASSERT(tmp != 0);

		if (tmp->type == CFG_BLOCK) {
			// If there are calls in it, mark them.
			if (tmp->data.block->calls) {
				Int j, size2;

				size2 = LPG_(smart_list_count)(tmp->data.block->calls);
				for (j = 0; j < size2; j++) {
					CFG* called_cfg = (CFG*) LPG_(smart_list_at)(tmp->data.block->calls, j);
					LPG_ASSERT(called_cfg != 0);

					mark_inside_main(called_cfg);
				}
			}
		}
	}
}

static
void mark_indirect(CFG* cfg, CfgNode* node) {
	LPG_ASSERT(cfg != 0);
	LPG_ASSERT(node != 0 && node->type == CFG_BLOCK);

	if (!node->data.block->indirect) {
		node->data.block->indirect = True;

		// Account for this indirection.
		cfg->stats.indirects++;
	}
}

void LPG_(cfg_set_inside_main)(CFG* cfg, Bool inside_main) {
	LPG_ASSERT(cfg != 0);

	// Ignore if we already marked this CFG inside main.
	if (cfg->inside_main) {
		LPG_ASSERT(inside_main);
		return;
	}

	// Ignore if we are not changing the status of the CFG.
	if (!inside_main)
		return;

	// All the nested CFG's called from this must also be inside main.
	LPG_(forall_cfg)(LPG_(clear_visited), True);
	mark_inside_main(cfg);
}

Bool LPG_(cfg_is_dirty)(CFG* cfg) {
	LPG_ASSERT(cfg != 0);
	return cfg->dirty;
}

Bool LPG_(cfg_is_visited)(CFG* cfg) {
	LPG_ASSERT(cfg != 0);
	return cfg->visited;
}

void LPG_(cfg_set_visited)(CFG* cfg, Bool visited) {
	LPG_ASSERT(cfg != 0);
	cfg->visited = visited;
}

Bool LPG_(cfg_is_complete)(CFG* cfg) {
	LPG_ASSERT(cfg != 0);
	return cfg->stats.indirects == 0 &&
		   cfg->stats.phantoms == 0;
}

CfgNode* LPG_(cfg_entry_node)(CFG* cfg) {
	LPG_ASSERT(cfg != 0);
	return cfg->entry;
}

CfgNode* LPG_(cfg_exit_node)(CFG* cfg) {
	LPG_ASSERT(cfg != 0);
	return cfg->exit;
}

CfgNode* LPG_(cfg_halt_node)(CFG* cfg) {
	LPG_ASSERT(cfg != 0);
	return cfg->halt;
}

SmartList* LPG_(cfg_nodes)(CFG* cfg) {
	LPG_ASSERT(cfg != 0);
	return cfg->nodes;
}

Bool LPG_(cfg_cmp)(CFG* cfg1, CFG* cfg2) {
	return (cfg1 != 0 && cfg2 != 0 && cfg1->addr == cfg2->addr);
}

Int LPG_(cfgnode_id)(CfgNode* node) {
	LPG_ASSERT(node != 0);
	return node->id;
}

enum CfgNodeType LPG_(cfgnode_type)(CfgNode* node) {
	LPG_ASSERT(node != 0);
	return node->type;
}

const HChar* LPG_(cfgnode_type2str)(enum CfgNodeType type, Bool lowercase) {
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

Addr LPG_(cfgnode_addr)(CfgNode* node) {
	LPG_ASSERT(node != 0);

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

Addr LPG_(cfgnode_size)(CfgNode* node) {
	LPG_ASSERT(node != 0);
	LPG_ASSERT(node->type == CFG_BLOCK);

	return node->data.block->size;
}

SmartList* LPG_(cfgnode_successors)(CfgNode* node) {
	LPG_ASSERT(node != 0);
	return node->info.successors.nodes;
}

SmartList* LPG_(cfgnode_predecessors)(CfgNode* node) {
	LPG_ASSERT(node != 0);
	return node->info.predecessors.nodes;
}

SmartList* LPG_(cfgnode_dominators)(CfgNode* node) {
	LPG_ASSERT(node != 0);
	return node->info.dominators;
}

void LPG_(cfgnode_set_dominators)(CfgNode* node, SmartList* dominators) {
	LPG_ASSERT(node != 0);

	if (node->info.dominators)
		LPG_(delete_smart_list)(node->info.dominators);

	node->info.dominators = dominators;
}

CfgNode* LPG_(cfgnode_immediate_dominator)(CfgNode* node) {
	LPG_ASSERT(node != 0);
	return node->info.idom;
}

void LPG_(cfgnode_set_immediate_dominator)(CfgNode* node, CfgNode* idom) {
	LPG_ASSERT(node != 0);
	node->info.idom = idom;
}

Bool LPG_(cfgnode_is_visited)(CfgNode* node) {
	LPG_ASSERT(node != 0);
	return node->visited;
}

void LPG_(cfgnode_set_visited)(CfgNode* node, Bool visited) {
	LPG_ASSERT(node != 0);
	node->visited = visited;
}

Bool LPG_(cfgnode_is_indirect)(CfgNode* node) {
	LPG_ASSERT(node != 0);
	return (node->type == CFG_BLOCK && node->data.block->indirect);
}

Bool LPG_(cfgnode_has_call_with_addr)(CfgNode* node, Addr addr) {
	Int i, size;

	LPG_ASSERT(node != 0 && node->type == CFG_BLOCK);
	LPG_ASSERT(addr != 0);

	if (node->data.block->calls) {
		size = LPG_(smart_list_count)(node->data.block->calls);
		for (i = 0; i < size; i++) {
			CFG* cfg = (CFG*) LPG_(smart_list_at)(node->data.block->calls, i);
			LPG_ASSERT(cfg != 0);

			if (cfg->addr == addr)
				return True;
		}
	}

	return False;
}

Bool LPG_(cfgnode_has_successor_with_addr)(CfgNode* node, Addr addr, Bool* is_virtual) {
	Int i, size;

	LPG_ASSERT(node != 0);
	LPG_ASSERT(addr != 0);

	size = LPG_(smart_list_count)(node->info.successors.nodes);
	for (i = 0; i < size; i++) {
		CfgNode* succ = (CfgNode*) LPG_(smart_list_at)(node->info.successors.nodes, i);
		LPG_ASSERT(succ != 0);

		// Ignore exit nodes. Entry nodes should never be successor.
		if (succ->type == CFG_EXIT || succ->type == CFG_HALT)
			continue;

		if (LPG_(cfgnode_addr)(succ) == addr) {
			if (is_virtual) {
				LPG_ASSERT(node->info.successors.flags != 0);
				*is_virtual = LPG_(bitset_get_pos)(node->info.successors.flags, i);
			}

			return True;
		}
	}

	return False;
}

Bool LPG_(cfgnodes_cmp)(CfgNode* node1, CfgNode* node2) {
	return (node1 && node2 && node1->id == node2->id);
}

static __inline__
Bool cfgnode_has_successors(CfgNode* node) {
	LPG_ASSERT(node != 0 && node->type == CFG_BLOCK);
	return LPG_(smart_list_count)(node->info.successors.nodes) > 0;
}

static __inline__
Bool cfgnode_has_calls(CfgNode* node) {
	LPG_ASSERT(node != 0 && node->type == CFG_BLOCK);
	return node->data.block->calls != 0 && LPG_(smart_list_count)(node->data.block->calls) > 0;
}

static __inline__
CfgInstrRef* get_succ_instr(CFG* cfg, CfgNode* from, Addr addr) {
	Int i, size;

	LPG_ASSERT(from->info.successors.nodes != 0);
	size = LPG_(smart_list_count)(from->info.successors.nodes);
	for (i = 0; i < size; i++) {
		CfgNode* node;
		CfgInstrRef* ref;

		node = (CfgNode*) LPG_(smart_list_at)(from->info.successors.nodes, i);
		LPG_ASSERT(node != 0);

		switch (node->type) {
			case CFG_BLOCK:
				ref = node->data.block->instrs.leader;
				LPG_ASSERT(ref != 0);
				break;
			case CFG_PHANTOM:
				ref = node->data.phantom;
				LPG_ASSERT(ref != 0);
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

	LPG_ASSERT(node != 0 && node->type == CFG_PHANTOM);

	ref = node->data.phantom;

	if (ref->instr->size == 0)
		ref->instr->size = new_size;
	else
		LPG_ASSERT(ref->instr->size == new_size);

	node->type = CFG_BLOCK;
	node->data.block = new_block(ref);
	ref->node = node;

	// Update the blocks and phantom stats.
	cfg->stats.blocks++;
	cfg->stats.phantoms--;
}

CfgNode* LPG_(cfgnode_set_block)(CFG* cfg, CfgNode* dangling, BB* bb, Int group_offset) {
	Addr base_addr, addr;
	UInt bb_idx, size;
	InstrGroupInfo group;
	Int accumulated_size;
	CfgInstrRef* curr_ref;

	LPG_ASSERT(cfg != 0);
	LPG_ASSERT(dangling != 0);
	LPG_ASSERT(dangling->type == CFG_ENTRY || dangling->type == CFG_BLOCK);
	LPG_ASSERT(bb != 0);

	// Get the group.
	LPG_ASSERT(group_offset >= 0 && group_offset < bb->groups_count);
	group = bb->groups[group_offset];

	// Find the first instruction index.
	bb_idx = group.bb_info.first_instr;
	base_addr = bb_addr(bb);

	// Last processed instruction for this group.
	curr_ref = 0;

	accumulated_size = 0;
	while (accumulated_size < group.group_size) {
		CfgInstrRef* next;

		if (curr_ref) {
			do {
				LPG_ASSERT(bb_idx < bb->instr_count);
				addr = base_addr + bb->instr[bb_idx].instr_offset;
				size = bb->instr[bb_idx].instr_size;

				LPG_ASSERT(curr_ref->instr->addr == addr);
				LPG_ASSERT(curr_ref->instr->size == size);

				accumulated_size += size;
				bb_idx++;

				curr_ref = curr_ref->next;
			} while (curr_ref && accumulated_size < group.group_size);
		} else {
			LPG_ASSERT(bb_idx < bb->instr_count);
			addr = base_addr + bb->instr[bb_idx].instr_offset;
			size = bb->instr[bb_idx].instr_size;

			// If we have a successor with this address.
			if ((next = get_succ_instr(cfg, dangling, addr))) {
				// If it is a phantom, convert it to a block.
				if (next->node->type == CFG_PHANTOM)
					phantom2block(cfg, next->node, size);

				LPG_ASSERT(ref_is_head(next));
				dangling = next->node;
			// Otherwise, check if this address already exists somewhere else in the CFG.
			} else if ((next = cfg_instr_find(cfg, addr))) {
				if (next->node->type == CFG_PHANTOM)
					phantom2block(cfg, next->node, size);
				else if (!ref_is_head(next))
					cfgnode_split(cfg, next);

				LPG_ASSERT(ref_is_head(next));
				add_edge2nodes(cfg, dangling, next->node, False);
				dangling = next->node;
			} else {
				next = new_instr_ref(LPG_(get_instr)(addr, size));
				// Check if we can append the instrution reference in the dangling block.
				if (dangling->type == CFG_BLOCK &&
					(dangling->data.block->instrs.tail->instr->addr + dangling->data.block->instrs.tail->instr->size) == addr &&
					!cfgnode_has_successors(dangling) && !cfgnode_has_calls(dangling)) {
					cfgnode_add_ref(cfg, dangling, next);
				} else {
					CfgNode* node = new_cfgnode_block(cfg, next);
					add_edge2nodes(cfg, dangling, node, False);
					dangling = node;
				}
			}

			curr_ref = next;
		}
	}
	LPG_ASSERT(accumulated_size == group.group_size);

	if (curr_ref && !ref_is_tail(curr_ref))
		dangling = cfgnode_split(cfg, curr_ref->next);

	return dangling;
}

void LPG_(cfgnode_set_phantom)(CFG* cfg, CfgNode* dangling, Addr to,
			LpgJumpKind jmpkind, Bool indirect) {
	CfgInstrRef* next;

	LPG_ASSERT(cfg != 0);
	LPG_ASSERT(dangling != 0);
	LPG_ASSERT(dangling->type == CFG_BLOCK);

	switch (jmpkind) {
		case jk_None:
			break;
		case jk_Jump:
			if (indirect) {
				// Mark the indirection and ignore the rest of code,
				// since we won't be able to create a phantom node for it.
				mark_indirect(cfg, dangling);
				return;
			}

			break;
		case jk_Call:
		case jk_Return:
			// Calls and returns are handled somewhere else, so we
			// can just skip them here.
			return;
		default:
			tl_assert(0);
	}

	// If we got here it is because we know the target address
	// and it is not an indirection.
	LPG_ASSERT(to != 0);
	LPG_ASSERT(indirect == False);

	// Ignore the phantom node if there is a call to this address.
	if (LPG_(cfgnode_has_call_with_addr)(dangling, to))
		return;

	// Get the successor if it has the leader with address "to"
	next = get_succ_instr(cfg, dangling, to);

	// If it is not existant, take some actions.
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
			next = new_instr_ref(LPG_(get_instr)(to, 0));
			new_cfgnode_phantom(cfg, next);
		}

		// Connect the nodes with a virtual edge.
		add_edge2nodes(cfg, dangling, next->node, True);
	}
}

void LPG_(cfgnode_set_call)(CFG* cfg, CfgNode* dangling, CFG* call, Bool indirect) {
	LPG_ASSERT(cfg != 0);
	LPG_ASSERT(dangling != 0);
	LPG_ASSERT(dangling->type == CFG_BLOCK);
	LPG_ASSERT(call != 0);

	if (indirect)
		mark_indirect(cfg, dangling);

	if (cfgnode_add_call(cfg, dangling, call)) {
		// If we are adding a call to a CFG that is inside main,
		// mark the called CFG as inside main as well.
		if (cfg->inside_main)
			LPG_(cfg_set_inside_main)(call, True);
	}
}

CfgNode* LPG_(cfgnode_set_exit)(CFG* cfg, CfgNode* dangling) {
	LPG_ASSERT(cfg != 0);
	LPG_ASSERT(dangling != 0);
	LPG_ASSERT(dangling->type == CFG_BLOCK);

	// Add the node if it is does not exist yet.
	add_edge2nodes(cfg, dangling, cfgnode_exit(cfg), False);
	return cfg->exit;
}

CfgNode* LPG_(cfgnode_set_halt)(CFG* cfg, CfgNode* dangling) {
	LPG_ASSERT(cfg != 0);
	LPG_ASSERT(dangling != 0);
	LPG_ASSERT(dangling->type == CFG_BLOCK);

	// Add the node if it is does not exist yet.
	add_edge2nodes(cfg, dangling, cfgnode_halt(cfg), False);
	return cfg->halt;
}

void LPG_(clean_visited_cfgnodes)(CFG* cfg) {
	Int i, size;

	LPG_ASSERT(cfg != 0);

	size = LPG_(smart_list_count)(cfg->nodes);
	for (i = 0; i < size; i++) {
		CfgNode* node = (CfgNode*) LPG_(smart_list_at)(cfg->nodes, i);
		LPG_ASSERT(node != 0);

		node->visited = False;
	}
}

void LPG_(check_cfg)(CFG* cfg) {
	Int i, j, size, size2;
	Int indirects;

	LPG_ASSERT(cfg != 0);

	LPG_ASSERT(cfg->exit != 0 || cfg->halt != 0);

	indirects = 0;
	size = LPG_(smart_list_count)(cfg->nodes);
	for (i = 0; i < size; i++) {
		CfgNode* node = (CfgNode*) LPG_(smart_list_at)(cfg->nodes, i);
		LPG_ASSERT(node != 0);

		switch (node->type) {
			case CFG_ENTRY:
				{
					CfgNode* tmp;
					CfgInstrRef* ref;

					// An entry node has no predecessors and only a single successor.
					LPG_ASSERT(node->info.predecessors.nodes == 0);
					LPG_ASSERT(node->info.successors.nodes != 0 && LPG_(smart_list_count)(node->info.successors.nodes) == 1);

					tmp = (CfgNode*) LPG_(smart_list_at)(node->info.successors.nodes, 0);
					LPG_ASSERT(tmp != 0 && tmp->type == CFG_BLOCK);

					// We must have at least one instruction per block.
					LPG_ASSERT(tmp->data.block->instrs.count > 0);

					// The first instruction address must match the cfg address.
					ref = tmp->data.block->instrs.leader;
					LPG_ASSERT(ref != 0);
					LPG_ASSERT(cfg->addr == ref->instr->addr);
				}

				break;
			case CFG_EXIT:
			case CFG_HALT:
				// An exit node has no successors and must have at least one predecessor.
				LPG_ASSERT(node->info.successors.nodes == 0);
				LPG_ASSERT(node->info.predecessors.nodes != 0 && LPG_(smart_list_count)(node->info.predecessors.nodes) > 0);

				break;
			case CFG_BLOCK:
				{
					Int total, count;
					CfgInstrRef* ref;
					CfgBlock* block;

					// A block node must have at least one predecessor and one sucessor.
					LPG_ASSERT(node->info.predecessors.nodes != 0 && LPG_(smart_list_count)(node->info.predecessors.nodes) > 0);
					LPG_ASSERT(node->info.successors.nodes != 0 && LPG_(smart_list_count)(node->info.successors.nodes) > 0);

					block = node->data.block;
					LPG_ASSERT(block != 0);

					count = 0;
					total = 0;
					ref = block->instrs.leader;
					while (ref) {
						if (ref->next) {
							LPG_ASSERT((ref->instr->addr + ref->instr->size) == ref->next->instr->addr);
						} else {
							LPG_ASSERT(block->instrs.tail == ref);
						}

						count++;
						total += ref->instr->size;

						ref = ref->next;
					}
					LPG_ASSERT(count == block->instrs.count);
					LPG_ASSERT(total == block->size);

					ref = block->instrs.tail;

					if (block->calls) {
						size2 = LPG_(smart_list_count)(block->calls);
						for (j = 0; j < size2; j++) {
							CFG* called = (CFG*) LPG_(smart_list_at)(block->calls, j);
							LPG_ASSERT(called != 0);

							LPG_ASSERT(!LPG_(cfgnode_has_successor_with_addr)(node, called->addr, 0));
						}
					}

					if (block->indirect)
						indirects++;
				}

				break;
			case CFG_PHANTOM:
				// A phantom must have at least one predecessor and no successors.
				LPG_ASSERT(node->info.predecessors.nodes != 0 && LPG_(smart_list_count)(node->info.predecessors.nodes) > 0);
				LPG_ASSERT(node->info.successors.nodes == 0 || LPG_(smart_list_count)(node->info.successors.nodes) == 0);

				// The phantom address must not be 0.
				LPG_ASSERT(node->data.phantom != 0);

				break;
			default:
				tl_assert(0);
		}

		if (node->info.successors.nodes != 0) {
			size2 = LPG_(smart_list_count)(node->info.successors.nodes);
			for (j = 0; j < size2; j++) {
				CfgNode* succ = (CfgNode*) LPG_(smart_list_at)(node->info.successors.nodes, j);
				LPG_ASSERT(succ != 0);

				// Check if the edge is virtual.
				if (LPG_(bitset_get_pos)(node->info.successors.flags, j)) {
					// The entry must not connect a successor with a virtual edge.
					LPG_ASSERT(node->type != CFG_ENTRY);
					// The successor exit node must not have a virtual edge.
					LPG_ASSERT(succ->type != CFG_EXIT && succ->type != CFG_HALT);
				} else {
					// A phantom node must not have a real (not virtual) edge.
					LPG_ASSERT(succ->type != CFG_PHANTOM);
				}
			}
		}
	}

	LPG_ASSERT(LPG_(smart_list_count)(cfg->nodes) ==
			(1 + (cfg->exit ? 1 : 0) + (cfg->halt ? 1 : 0) + cfg->stats.blocks + cfg->stats.phantoms));
	LPG_ASSERT(indirects == cfg->stats.indirects);

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

	LPG_ASSERT(out != 0);
	LPG_ASSERT(cfg != 0);

	VG_(fprintf)(out, "digraph \"0x%lx\" {\n", cfg->addr);

	VG_(fprintf)(out, "  label = \"0x%lx (", cfg->addr);
	if (!cfg->fdesc)
		LPG_(cfg_build_fdesc)(cfg);
	if (cfg->fdesc) {
		LPG_(fprint_fdesc)(out, cfg->fdesc);
	} else {
		VG_(fprintf)(out, "unknown");
	}
	VG_(fprintf)(out, ")\"\n");
	VG_(fprintf)(out, "  labelloc = \"t\"\n");
	VG_(fprintf)(out, "  node[shape=record]\n\n");

	unknown = 1;
	size = LPG_(smart_list_count)(cfg->nodes);
	for (i = 0; i < size; i++) {
		CfgNode* node;

		node = (CfgNode*) LPG_(smart_list_at)(cfg->nodes, i);
		LPG_ASSERT(node != 0);

		if (node->type == CFG_ENTRY) {
			VG_(fprintf)(out, "  %s [label=\"\",width=0.3,height=0.3,shape=circle,fillcolor=black,style=filled]\n",
					LPG_(cfgnode_type2str)(CFG_ENTRY, False));
		} else if (node->type == CFG_EXIT) {
			VG_(fprintf)(out, "  %s [label=\"\",width=0.3,height=0.3,shape=circle,fillcolor=black,style=filled,peripheries=2]\n",
					LPG_(cfgnode_type2str)(CFG_EXIT, False));
		} else if (node->type == CFG_HALT) {
			VG_(fprintf)(out, "  %s [label=\"\",width=0.3,height=0.3,shape=square,fillcolor=black,style=filled,peripheries=2]\n",
					LPG_(cfgnode_type2str)(CFG_HALT, False));
		} else if (node->type == CFG_BLOCK) {
			VG_(fprintf)(out, "  \"0x%lx\" [label=\"{\n", LPG_(cfgnode_addr)(node));
			VG_(fprintf)(out, "     0x%lx [%u]\\l\n",
					node->data.block->addr, node->data.block->size);

			if (detailed) {
				CfgInstrRef* ref;

				VG_(fprintf)(out, "     | [instrs]\\l\n");

				ref = node->data.block->instrs.leader;
				LPG_ASSERT(ref != 0);

				while (ref) {
					VG_(fprintf)(out, "     &nbsp;&nbsp;0x%lx \\<+%u\\>: ",
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

				size2 = LPG_(smart_list_count)(node->data.block->calls);
				for (j = 0; j < size2; j++) {
					CFG* called_cfg;
					HChar* desc;

					called_cfg = (CFG*) LPG_(smart_list_at)(node->data.block->calls, j);
					LPG_ASSERT(called_cfg != 0);

					VG_(fprintf)(out, "     &nbsp;&nbsp;0x%lx (", called_cfg->addr);

					desc = LPG_(fdesc2str)(called_cfg->fdesc);
					fprintf_escape(out, desc);
					LPG_FREE(desc);

					VG_(fprintf)(out, ")\\l\n");
				}
			}

			if (detailed) {
				if (node->info.idom) {
					VG_(fprintf)(out, "     | [idom]\\l\n");
					VG_(fprintf)(out, "     &nbsp;&nbsp;");
					if (node->info.idom->type == CFG_ENTRY)
						VG_(fprintf)(out, "%s", LPG_(cfgnode_type2str)(node->info.idom->type, False));
					else
						VG_(fprintf)(out, "0x%lx", LPG_(cfgnode_addr)(node->info.idom));
					VG_(fprintf)(out, "\\l\n");
				}
			}

			VG_(fprintf)(out, "  }\"]\n");
		} else if (node->type == CFG_PHANTOM) {
			VG_(fprintf)(out, "  \"0x%lx\" [label=\"{\n", LPG_(cfgnode_addr)(node));
			VG_(fprintf)(out, "     0x%lx\\l\n",
					node->data.phantom->instr->addr);
			VG_(fprintf)(out, "  }\", style=dashed]\n");
		} else {
			tl_assert(0);
		}

		if (node->info.successors.nodes != 0) {
			LPG_ASSERT(node->info.successors.flags != 0);

			size2 = LPG_(smart_list_count)(node->info.successors.nodes);
			for (j = 0; j < size2; j++) {
				CfgNode* succ = (CfgNode*) LPG_(smart_list_at)(node->info.successors.nodes, j);
				LPG_ASSERT(succ != 0);
				LPG_ASSERT(succ->type != CFG_ENTRY);

				// If it has successors, it shouldn't be an exit.
				LPG_ASSERT(node->type != CFG_EXIT && node->type != CFG_HALT);

				if (node->type == CFG_ENTRY)
					VG_(fprintf)(out, "  %s -> ", LPG_(cfgnode_type2str)(node->type, False));
				else
					VG_(fprintf)(out, "  \"0x%lx\" -> ", LPG_(cfgnode_addr)(node));

				if (succ->type == CFG_EXIT || succ->type == CFG_HALT)
					VG_(fprintf)(out, "%s\n", LPG_(cfgnode_type2str)(succ->type, False));
				else
					VG_(fprintf)(out, "\"0x%lx\"", LPG_(cfgnode_addr)(succ));

				// Check the edge dashed if the edge is virtual.
				if (LPG_(bitset_get_pos)(node->info.successors.flags, j)) {
					LPG_ASSERT(succ->type != CFG_EXIT && succ->type != CFG_HALT);
					VG_(fprintf)(out, " [style=dashed]");
				} else {
					// If the edge is not virtual, the succ cannot be a phantom node.
					LPG_ASSERT(succ->type != CFG_PHANTOM);
				}

				VG_(fprintf)(out, "\n");
			}
		}

		if (LPG_(cfgnode_is_indirect)(node)) {
			VG_(fprintf)(out, "  \"Unknown%d\" [label=\"?\", shape=none]\n", unknown);
			VG_(fprintf)(out, "  \"0x%lx\" -> \"Unknown%d\" [style=dashed]\n",
					LPG_(cfgnode_addr)(node), unknown);
			unknown++;
		}
	}
	VG_(fprintf)(out, "}\n");
}

void LPG_(fprint_cfg)(VgFile* out, CFG* cfg) {
	fprint_cfg(out, cfg, False);
}

void LPG_(fprint_detailed_cfg)(VgFile* out, CFG* cfg) {
	fprint_cfg(out, cfg, True);
}

static
void write_cfg(CFG* cfg) {
	Int i, size;
	Int j, size2;

	LPG_ASSERT(cfg != 0);
	LPG_ASSERT(fp != 0);

	VG_(fprintf)(fp, "[cfg 0x%lx %s \"", cfg->addr, (cfg->inside_main ? "true" : "false"));
	if (!cfg->fdesc)
		LPG_(cfg_build_fdesc)(cfg);

	if (cfg->fdesc)
		LPG_(fprint_fdesc)(fp, cfg->fdesc);
	else
		VG_(fprintf)(fp, "unknown");
	VG_(fprintf)(fp, "\" %s]\n", (LPG_(cfg_is_complete)(cfg) ? "true" : "false"));

	size = LPG_(smart_list_count)(cfg->nodes);
	for (i = 0; i < size; i++) {
		CfgNode* node;
		CfgInstrRef* ref;

		node = (CfgNode*) LPG_(smart_list_at)(cfg->nodes, i);
		LPG_ASSERT(node != 0);

		// We only write block nodes.
		if (node->type != CFG_BLOCK)
			continue;

		VG_(fprintf)(fp, "[node 0x%lx ", cfg->addr);

		if (node->type == CFG_BLOCK) {
			VG_(fprintf)(fp, "0x%lx ", node->data.block->addr);

			ref = node->data.block->instrs.leader;
			LPG_ASSERT(ref != 0);

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
				size2 = LPG_(smart_list_count)(node->data.block->calls);
				for (j = 0; j < size2; j++) {
					CFG* called = (CFG*) LPG_(smart_list_at)(node->data.block->calls, j);
					LPG_ASSERT(called != 0);

					if (j > 0)
						VG_(fprintf)(fp, " ");

					VG_(fprintf)(fp, "0x%lx", called->addr);
				}
			}
			VG_(fprintf)(fp, "] ");

			VG_(fprintf)(fp, "%s ", node->data.block->indirect ? "true" : "false");

			VG_(fprintf)(fp, "[");
			size2 = LPG_(smart_list_count)(node->info.successors.nodes);
			for (j = 0; j < size2; j++) {
				CfgNode* succ = (CfgNode*) LPG_(smart_list_at)(node->info.successors.nodes, j);
				LPG_ASSERT(succ != 0);

				if (j > 0)
					VG_(fprintf)(fp, " ");

				switch (succ->type) {
					case CFG_EXIT:
					case CFG_HALT:
						VG_(fprintf)(fp, "%s", LPG_(cfgnode_type2str)(succ->type, True));
						break;
					case CFG_BLOCK:
					case CFG_PHANTOM:
						VG_(fprintf)(fp, "0x%lx", LPG_(cfgnode_addr)(succ));
						break;
					default:
						tl_assert(0);
				}

				// If the edge is virtual, append a marker after it.
				if (LPG_(bitset_get_pos)(node->info.successors.flags, j)) {
					LPG_ASSERT(succ->type != CFG_EXIT && succ->type != CFG_HALT);
					VG_(fprintf)(fp, "\'");
				} else {
					LPG_ASSERT(succ->type != CFG_PHANTOM);
				}
			}
			VG_(fprintf)(fp, "]");
		} else {
			tl_assert(0);
		}

		VG_(fprintf)(fp, "]\n");
	}
}

void LPG_(write_cfgs)(VgFile *out_fp) {
	LPG_ASSERT(out_fp != 0);

	fp = out_fp;
	LPG_(forall_cfg)(write_cfg, True);
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

        LPG_ASSERT(idx >= 0 && idx < ((sizeof(token.text) / sizeof(HChar))-1));
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
				} else if (c == '\'') {
					token.text[idx++] = c;
					token.type = TKN_PRIME;
					state = 8;
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

void LPG_(read_cfgs)(Int fd) {
	Bool has;
	CFG* cfg;
	CfgNode* node;
	CfgInstrRef* ref;
	Addr addr;
	Int size;

	while (next_token(fd)) {
		LPG_ASSERT(token.type == TKN_BRACKET_OPEN);

		has = next_token(fd);
		LPG_ASSERT(has);

		switch (token.type) {
			case TKN_CFG:
				has = next_token(fd);
				LPG_ASSERT(has && token.type == TKN_ADDR);

				cfg = LPG_(get_cfg)(token.data.addr);
				LPG_ASSERT(cfg != 0);

				has = next_token(fd);
				LPG_ASSERT(has && token.type == TKN_BOOL);
				cfg->inside_main = token.data.bool;

				has = next_token(fd);
				LPG_ASSERT(has && token.type == TKN_TEXT);
				cfg->fdesc = LPG_(str2fdesc)(token.text);

				has = next_token(fd);
				LPG_ASSERT(has && token.type == TKN_BOOL);

				break;
			case TKN_NODE:
				has = next_token(fd);
				LPG_ASSERT(has && token.type == TKN_ADDR);

				cfg = LPG_(get_cfg)(token.data.addr);
				LPG_ASSERT(cfg != 0);

				has = next_token(fd);
				LPG_ASSERT(has && token.type == TKN_ADDR);
				addr = token.data.addr;

				// Search for the first instruction in the block reference.
				ref = cfg_instr_find(cfg, addr);

				has = next_token(fd);
				LPG_ASSERT(has && token.type == TKN_BRACKET_OPEN);

				has = next_token(fd);
				LPG_ASSERT(has && token.type == TKN_NUMBER);
				size = token.data.number;

				// If the reference exists, then it must be a phantom
				// node and we will convert it to a block node.
				if (ref) {
					LPG_ASSERT(ref->node->type == CFG_PHANTOM);
					node = ref->node;
					phantom2block(cfg, node, size);
				// Otherwise, we will create the block node.
				} else {
					ref = new_instr_ref(LPG_(get_instr)(addr, size));
					node = new_cfgnode_block(cfg, ref);
				}
				LPG_ASSERT(node->type == CFG_BLOCK);

				// If the address match the CFG's addr, then it is the entry block.
				if (addr == cfg->addr)
					add_edge2nodes(cfg, cfg->entry, node, False);

				addr += size;

				has = next_token(fd);
				LPG_ASSERT(has);

				while (token.type == TKN_NUMBER) {
					size = token.data.number;

					cfgnode_add_ref(cfg, node,
							new_instr_ref(LPG_(get_instr)(addr, size)));

					addr += size;

					has = next_token(fd);
					LPG_ASSERT(has);
				}

				LPG_ASSERT(has && token.type == TKN_BRACKET_CLOSE);

				has = next_token(fd);
				LPG_ASSERT(has && token.type == TKN_BRACKET_OPEN);

				has = next_token(fd);
				LPG_ASSERT(has);

				while (token.type == TKN_ADDR) {
					if (node->data.block->calls == 0)
						node->data.block->calls = LPG_(new_smart_list)(1);

					LPG_(smart_list_add)(node->data.block->calls, LPG_(get_cfg)(token.data.addr));

					has = next_token(fd);
					LPG_ASSERT(has);
				}

				LPG_ASSERT(has && token.type == TKN_BRACKET_CLOSE);

				has = next_token(fd);
				LPG_ASSERT(has && token.type == TKN_BOOL);

				if (token.data.bool)
					mark_indirect(cfg, node);

				has = next_token(fd);
				LPG_ASSERT(has && token.type == TKN_BRACKET_OPEN);

				has = next_token(fd);
				LPG_ASSERT(has);

				while (token.type == TKN_EXIT ||
					   token.type == TKN_HALT ||
					   token.type == TKN_ADDR) {
					switch (token.type) {
						case TKN_EXIT:
							add_edge2nodes(cfg, node, cfgnode_exit(cfg), False);

							has = next_token(fd);
							LPG_ASSERT(has);
							break;
						case TKN_HALT:
							add_edge2nodes(cfg, node, cfgnode_halt(cfg), False);

							has = next_token(fd);
							LPG_ASSERT(has);
							break;
						case TKN_ADDR:
							ref = cfg_instr_find(cfg, token.data.addr);
							if (!ref) {
								ref = new_instr_ref(LPG_(get_instr)(token.data.addr, 0));
								new_cfgnode_phantom(cfg, ref);
							}

							has = next_token(fd);
							LPG_ASSERT(has);

							if (token.type == TKN_PRIME) {
								add_edge2nodes(cfg, node, ref->node, True);

								has = next_token(fd);
								LPG_ASSERT(has);
							} else {
								add_edge2nodes(cfg, node, ref->node, False);
							}

							break;
						default:
							tl_assert(0);
					}
				}

				LPG_ASSERT(has && token.type == TKN_BRACKET_CLOSE);

				break;
			default:
				tl_assert(0);
		}

		has = next_token(fd);
		LPG_ASSERT(has && token.type == TKN_BRACKET_CLOSE);
	}

	// Check the CFG's
	LPG_(forall_cfg)(LPG_(check_cfg), True);
}

static
Bool cmp_strings(HChar* str1, HChar* str2) {
	return str1 != 0 && str2 != 0 && VG_(strcmp)(str1, str2) == 0;
}

void LPG_(dump_cfg)(CFG* cfg) {
	VgFile* out;
	HChar filename[256];
	HChar* funct;

	LPG_ASSERT(cfg != 0);

	funct = cfg->fdesc ? LPG_(fdesc_function_name)(cfg->fdesc) : 0;
	if (LPG_(clo).dump_cfgs.all ||
		(LPG_(clo).dump_cfgs.addrs != 0 &&
				LPG_(smart_list_contains)(LPG_(clo).dump_cfgs.addrs, (void*) cfg->addr, 0)) ||
		(LPG_(clo).dump_cfgs.fnames != 0 && funct != 0 &&
				LPG_(smart_list_contains)(LPG_(clo).dump_cfgs.fnames, funct,
						(Bool (*)(void*, void*)) cmp_strings))) {
		VG_(snprintf)(filename, sizeof(filename)/sizeof(HChar),
				"%s/cfg-0x%lx.dot", LPG_(clo).dump_cfgs.dir, cfg->addr);

		out = VG_(fopen)(filename, VKI_O_WRONLY|VKI_O_TRUNC, 0);
		if (out == NULL) {
			out = VG_(fopen)(filename, VKI_O_CREAT|VKI_O_WRONLY,
					VKI_S_IRUSR|VKI_S_IWUSR);
		}
		LPG_ASSERT(out != 0);

		LPG_(fprint_detailed_cfg)(out, cfg);

		VG_(fclose)(out);
	}
}

void LPG_(forall_cfg)(void (*func)(CFG*), Bool all) {
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

void LPG_(clear_visited)(CFG* cfg) {
	LPG_ASSERT(cfg != 0);

	cfg->visited = False;
}
