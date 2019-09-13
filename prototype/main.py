#!/usr/bin/env python3

import sys

from group import *
from cfg import *
from machine import *
from config import *
from state import *

def write_cfg(cfg, dangling):
	global total
	with open("cfg%03d.dot" % total, "w") as fd:
		fd.write(cfg.dot(dangling))
	total = total + 1

def process_tail(state, instr):
	# For jump instruction, do nothing since it
	# will be handled in the next iteration when
	# processing the next group.
	if isinstance(instr.type, JumpType):
		write_cfg(state.current.cfg, state.current.dangling)

	# For branch instruction, we must take into consideration
	# the addresses of the fallthrough and jump (if direct).
	elif isinstance(instr.type, BranchType):
		# The possible target addresses.
		addrs = [instr.type.fallthrough]
		if instr.type.direct:
			addrs.append(instr.type.target)

		# for each of these addresses.
		for addr in addrs:
				# Search for a node that contains an instruction
				# with this address.
				node = state.current.cfg.node_with_addr(addr)

				# If exists, node can be either phantom or basic block.
				if node:
					# If it is a basic block, split the node if the
					# address does not match the leader instruction.
					if isinstance(node, BasicBlock) and node.group.leader.addr != addr:
						node = state.current.cfg.split(node, addr)
				# Otherwise, create a phantom node with this address.
				else:
					node = state.current.cfg.add_node(Phantom(addr))

				# Connect the dangling with this node if it not a
				# successor already.
				if not (node in state.current.cfg.succs(state.current.dangling)):
					state.current.cfg.add_edge(Edge(state.current.dangling, node))

		write_cfg(state.current.cfg, state.current.dangling)

	# For call instruction, save the current cfg with the dangling node.
	# Later, save the callee node and mark the CFG as nil for delayed discovery.
	elif isinstance(instr.type, CallType):
		write_cfg(state.current.cfg, state.current.dangling)

		# Append the current cfg and dangling node in the call stack.
		state.callstack.push(state.current)

		# Save the node with the call.
		state.caller = state.current.dangling

	# For return instruction, connect dangling with the exit node if not
	# a successor already. Later, restore the previous cfg and dangling
	# from the callstack.
	elif isinstance(instr.type, ReturnType):
		write_cfg(state.current.cfg, state.current.dangling)

		# Connect dangling to the exit node if not existent.
		if not (state.current.cfg.exit in state.current.cfg.succs(state.current.dangling)):
			state.current.cfg.add_edge(Edge(state.current.dangling, state.current.cfg.exit))

		write_cfg(state.current.cfg, state.current.cfg.exit)

		# Restore call function is the call stack is not empty.
		if state.callstack:
			state.current = state.callstack.pop()
		# Otherwise, it means it is the end of the program.
		else:
			exit(0)

	# For return instruction, connect dangling with the terminate node if not
	# a successor already. Do the same for all nodes in the call stack.
	elif isinstance(instr.type, HaltType):
		write_cfg(state.current.cfg, state.current.dangling)

		# Connect dangling to the terminate node if not existent.
		if not (state.current.cfg.term in state.current.cfg.succs(state.current.dangling)):
			state.current.cfg.add_edge(Edge(state.current.dangling, state.current.cfg.term))

		write_cfg(state.current.cfg, state.current.cfg.term)

		# Connect each dangling node in the call stack with the
		# termination node.
		while state.callstack:
			state.current = state.callstack.pop()
			if not (state.current.cfg.term in state.current.cfg.succs(state.current.dangling)):
				state.current.cfg.add_edge(Edge(state.current.dangling, state.current.cfg.term))

			write_cfg(state.current.cfg, state.current.cfg.term)

		# End of the program.
		exit(0)

	# Should never be a standard instruction.
	else:
		assert False, "unreachable code"

	return state

def process_group(cfg, dangling, group):
	# Current instruction in the dangling node can be any,
	# from leader to tail. We use it to match the
	# instructions in the group in sequence.
	#
	# A nil value indicates that the current
	# instruction in the group must be a successor of
	# the dangling node. The first instruction of the
	# group must always be a successor, hence the nil value.
	curr_instr = None

	# For each instruction in the group.
	for instr in group.instrs():
		# If there is a current instruction, it must match
		# the instruction being processed.
		if curr_instr:
			assert curr_instr == instr
		# If current instruction is nil, find the successor
		# of the dangling that matches the instruction in the group.
		# Create, split or transform node if necessary.
		else:
			# Search if there is a node that contains an instruction
			# with the address of the instruction.
			node = cfg.node_with_addr(instr.addr)

			# If exists, then this node is or will be a successor of the dangling node.
			if node:
				# If it is a phantom node, transform to a basic block.
				if isinstance(node, Phantom):
					node = cfg.phantom2basicblock(node, BasicBlock(Group(instr)))
				# Otherwise it must be a basic block.
				# If instruction is not the leader.
				elif node.group.leader != instr:
					node = cfg.split(node, instr.addr)

				# The leader of this node must match the instruction.
				assert node.group.leader == instr

				# Make it a successor of the dangling not if it is not already.
				if not (node in cfg.succs(dangling)):
					cfg.add_edge(Edge(dangling, node))

				# Make this node the dangling node.
				dangling = node
			# If it does not exist, the instruction is new and we can:
			# (1) append it to the dangling node (if possible); or
			# (2) create a new block with it as the head instruction that
			# will be connected to the dangling. This block will be the
			# new dangling node.
			else:
				# Append the instruction if possible.
				# It must not be the first instruction in the group,
				# the node must be a basic block without successors and calls.
				if instr != group.leader and isinstance(dangling, BasicBlock) and \
						(not dangling.calls) and (not cfg.succs(dangling)):
					# Since the instructions are in sequence, the instruction must come
					# immediately after the tail of the dangling node.
					assert (dangling.group.tail.addr + dangling.group.tail.size) == instr.addr

					# Add the instruction making it the new tail.
					dangling.group.add_instr(instr)
				# Create a new block, connect the dangling to it and
				# make it the new dangling node.
				else:
					node = cfg.add_node(BasicBlock(Group(instr)))
					cfg.add_edge(Edge(dangling, node))
					dangling = node

		# Make the next instruction in the dangling block the current for the next iteration.
		# It can be nil if there are no more instructions left.
		curr_instr = dangling.group.next(instr)

	return dangling

def process_program(state, machine):
	# The start CFG with the first instruction address
	# that will be executed. Set the dangling node with
	# the entry node.
	state.current.cfg = CFG.instance(machine.start_addr())
	state.current.dangling = state.current.cfg.entry

	write_cfg(state.current.cfg, state.current.dangling)

	# The next group of instruction that will be executed.
	for group in machine.run():
		# Delayed discovery of the cfg.
		if state.caller:
			# Delayed discovered of CFG using the address
			# of the group leader. Set the dangling node
			# with this CFG's entry node.
			state.current.cfg = CFG.instance(group.leader.addr)
			state.current.dangling = state.current.cfg.entry

			write_cfg(state.current.cfg, state.current.dangling)

			# Store the cfg in the call list of the caller node.
			if not state.current.cfg in state.caller.calls:
				state.caller.add_call(state.current.cfg)
			state.caller = None

		# Check if we processed this group from this dangling point.
		idx = group.leader.addr % CACHE_SIZE
		cached_group, cached_dangling = state.current.dangling.cache[idx]
		if cached_group == group:
			# In this case, just use the next dangling from the start.
			state.current.dangling = cached_dangling
		else:
			# Save the dangling pointer before processing the group.
			prev_dangling = state.current.dangling
			# Process the group and update dangling.
			state.current.dangling = process_group(state.current.cfg, state.current.dangling, group)
			# Add the new dangling node to the cache.
			prev_dangling.cache[idx] = (group, state.current.dangling)

		# Get the last processed instruction from the group.
		state = process_tail(state, group.tail)

	# If the program didn't exit, do a gracefully exit
	assert isinstance(state.current.dangling, BasicBlock);

	# Connect dangling to the exit node if not existent.
	if not (state.current.cfg.exit in state.current.cfg.succs(state.current.dangling)):
		state.current.cfg.add_edge(Edge(state.current.dangling, state.current.cfg.exit))

	write_cfg(state.current.cfg, state.current.cfg.exit)

	# Connect each dangling node in the call stack with the exit node.
	while state.callstack:
		state.current = state.callstack.pop()
		if not (state.current.cfg.exit in state.current.cfg.succs(state.current.dangling)):
			state.current.cfg.add_edge(Edge(state.current.dangling, state.current.cfg.exit))

		write_cfg(state.current.cfg, state.current.cfg.exit)

if len(sys.argv) != 2:
	print("Usage: %s [Program Description]" % sys.argv[0])
	exit(1)

total = 1
process_program(State(), Machine(sys.argv[1]))
