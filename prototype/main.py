#!/usr/bin/env python3

import sys

from group import *
from cfg import *
from machine import *
from config import *
from state import *

def write_cfg(cfg, working):
	global total
	with open("cfg%03d.dot" % total, "w") as fd:
		fd.write(cfg.dot(working))
	total = total + 1

def process_tail(state, instr):
	# For jump instruction, do nothing since it
	# will be handled in the next iteration when
	# processing the next group.
	if isinstance(instr.type, JumpType):
		write_cfg(state.current.cfg, state.current.working)

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
				node = state.current.cfg.find_node_with_addr(addr)

				# If exists, node can be either phantom or basic block.
				if node:
					# If it is a basic block, split the node if the
					# address does not match the leader instruction.
					if isinstance(node, BasicBlock) and node.group.leader.addr != addr:
						node = state.current.cfg.split(node, addr)
				# Otherwise, create a phantom node with this address.
				else:
					node = state.current.cfg.add_node(Phantom(addr))

				# Connect the working with this node if it not a
				# successor already.
				if not (node in state.current.cfg.succs(state.current.working)):
					state.current.cfg.add_edge(Edge(state.current.working, node))

		write_cfg(state.current.cfg, state.current.working)

	# For call instruction, save the current cfg with the working node.
	# Later, save the callee node and mark the CFG as nil for delayed discovery.
	elif isinstance(instr.type, CallType):
		write_cfg(state.current.cfg, state.current.working)

		# Append the current cfg and working node in the call stack.
		state.callstack.push(state.current)

		# Save the node with the call.
		state.pending = state.current.working

	# For return instruction, connect working with the exit node if not
	# a successor already. Later, restore the previous cfg and working
	# from the callstack.
	elif isinstance(instr.type, ReturnType):
		write_cfg(state.current.cfg, state.current.working)

		# Connect working to the exit node if not existent.
		if not (state.current.cfg.exit in state.current.cfg.succs(state.current.working)):
			state.current.cfg.add_edge(Edge(state.current.working, state.current.cfg.exit))

		write_cfg(state.current.cfg, state.current.cfg.exit)

		# Restore call function is the call stack is not empty.
		if state.callstack:
			state.current = state.callstack.pop()
		# Otherwise, it means it is the end of the program.
		else:
			exit(0)

	# For return instruction, connect working with the terminate node if not
	# a successor already. Do the same for all nodes in the call stack.
	elif isinstance(instr.type, HaltType):
		write_cfg(state.current.cfg, state.current.working)

		# Connect working to the terminate node if not existent.
		if not (state.current.cfg.term in state.current.cfg.succs(state.current.working)):
			state.current.cfg.add_edge(Edge(state.current.working, state.current.cfg.term))

		write_cfg(state.current.cfg, state.current.cfg.term)

		# Connect each working node in the call stack with the
		# termination node.
		while state.callstack:
			state.current = state.callstack.pop()
			if not (state.current.cfg.term in state.current.cfg.succs(state.current.working)):
				state.current.cfg.add_edge(Edge(state.current.working, state.current.cfg.term))

			write_cfg(state.current.cfg, state.current.cfg.term)

		# End of the program.
		exit(0)

	# Should never be a standard instruction.
	else:
		assert False, "unreachable code"

	return state

def process_group(cfg, working, group):
	# Current instruction in the working node can be any,
	# from leader to tail. We use it to match the
	# instructions in the group in sequence.
	#
	# A nil value indicates that the current
	# instruction in the group must be a successor of
	# the working node. The first instruction of the
	# group must always be a successor, hence the nil value.
	curr_instr = None

	# For each instruction in the group.
	for instr in group.instrs():
		# If there is a current instruction, it must match
		# the instruction being processed.
		if curr_instr:
			assert curr_instr == instr
		# If current instruction is nil, find the successor
		# of the working that matches the instruction in the group.
		# Create, split or transform node if necessary.
		else:
			# Search if there is a node that contains an instruction
			# with the address of the instruction.
			node = cfg.find_node_with_addr(instr.addr)

			# If exists, then this node is or will be a successor of the working node.
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

				# Make it a successor of the working not if it is not already.
				if not (node in cfg.succs(working)):
					cfg.add_edge(Edge(working, node))

				# Make this node the working node.
				working = node
			# If it does not exist, the instruction is new and we can:
			# (1) append it to the working node (if possible); or
			# (2) create a new block with it as the head instruction that
			# will be connected to the working. This block will be the
			# new working node.
			else:
				# Append the instruction if possible.
				# It must not be the first instruction in the group,
				# the node must be a basic block without successors and calls.
				if instr != group.leader and isinstance(working, BasicBlock) and \
						(not working.calls) and (not cfg.succs(working)):
					# Since the instructions are in sequence, the instruction must come
					# immediately after the tail of the working node.
					assert (working.group.tail.addr + working.group.tail.size) == instr.addr

					# Add the instruction making it the new tail.
					working.group.add_instr(instr)
				# Create a new block, connect the working to it and
				# make it the new working node.
				else:
					node = cfg.add_node(BasicBlock(Group(instr)))
					cfg.add_edge(Edge(working, node))
					working = node

		# Make the next instruction in the working block the current for the next iteration.
		# It can be nil if there are no more instructions left.
		curr_instr = working.group.next(instr)

	return working

def process_program(mapping, machine):
	# The start CFG with the first instruction address
	# that will be executed. Set the working node with
	# the entry node.
	addr = machine.start_addr()
	initial = mapping.setdefault(addr, CFG(addr))
	state = State((initial, initial.entry), [], None)

	write_cfg(state.current.cfg, state.current.working)

	# The next group of instruction that will be executed.
	for group in machine.run():
		# Delayed discovery of the cfg.
		if state.pending:
			# Delayed discovered of CFG using the address
			# of the group leader. Set the working node
			# with this CFG's entry node.
			# called = CFG.instance(group.leader.addr)
			addr = group.leader.addr
			called = mapping.setdefault(addr, CFG(addr))

			# Store the cfg in the call list of the pending node.
			if not (called in state.pending.calls):
				state.pending.add_call(called)
			state.pending = None

			state.current = (called, called.entry)
			write_cfg(state.current.cfg, state.current.working)

		# Check if we processed this group from this working point.
		idx = group.leader.addr % CACHE_SIZE
		cached_group, cached_working = state.current.working.cache[idx]
		if cached_group == group:
			# In this case, just use the next working from the start.
			state.current.working = cached_working
		else:
			# Save the working pointer before processing the group.
			prev_working = state.current.working
			# Process the group and update working.
			state.current.working = process_group(state.current.cfg, state.current.working, group)
			# Add the new working node to the cache.
			prev_working.cache[idx] = (group, state.current.working)

		# Get the last processed instruction from the group.
		state = process_tail(state, group.tail)

	# If the program didn't exit, do a gracefully exit
	assert isinstance(state.current.working, BasicBlock);

	# Connect working to the exit node if not existent.
	if not (state.current.cfg.exit in state.current.cfg.succs(state.current.working)):
		state.current.cfg.add_edge(Edge(state.current.working, state.current.cfg.exit))

	write_cfg(state.current.cfg, state.current.cfg.exit)

	# Connect each working node in the call stack with the exit node.
	while state.callstack:
		state.current = state.callstack.pop()
		if not (state.current.cfg.exit in state.current.cfg.succs(state.current.working)):
			state.current.cfg.add_edge(Edge(state.current.working, state.current.cfg.exit))

		write_cfg(state.current.cfg, state.current.cfg.exit)

	return mapping

if len(sys.argv) != 2:
	print("Usage: %s [Program Description]" % sys.argv[0])
	exit(1)

total = 1
process_program({}, Machine(sys.argv[1]))
