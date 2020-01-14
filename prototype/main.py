#!/usr/bin/env python3

import sys

from group import *
from cfg import *
from machine import *
from config import *
from state import *

__DEBUGGING__ = True
__CACHING__   = False

def write_cfg(cfg, prefix = "cfg", working = None):
	global total
	with open("%s%03d.dot" % (prefix, total), "w") as fd:
		fd.write(cfg.dot(working))
	total = total + 1

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

				# Make it a successor of the working or update count.
				cfg.add_edge(working, node, 1)

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
					cfg.add_edge(working, node, 1)
					working = node

		# Make the next instruction in the working block the current for the next iteration.
		# It can be nil if there are no more instructions left.
		curr_instr = working.group.next(instr)

	return working

def process_type(mapping, state, type, target_addr):
	# For jump instruction, do nothing since it
	# will be handled in the next iteration when
	# processing the next group.
	if isinstance(type, JumpType):
		pass

	# For branch instruction, we must take into consideration
	# the addresses of the fallthrough and jump (if direct).
	elif isinstance(type, BranchType):
		# The possible target addresses.
		addrs = [type.fallthrough]
		if type.direct:
			addrs.append(type.target)

		# for each of these addresses.
		for addr in addrs:
			if addr != target_addr:
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

				# Connect the working with this node or update count.
				state.current.cfg.add_edge(state.current.working, node, 0)

		if __DEBUGGING__:
			write_cfg(state.current.cfg, "step", state.current.working)

	# For call instruction, find the target cfg and added to the call list of
	# the working node. Then. save the current state with the return address
	# in the call stack and set the new current state with the called cfg
	# and its entry node.
	elif isinstance(type, CallType):
		# Find the called CFG.
		called = mapping.setdefault(target_addr, CFG(target_addr))

		# Add the called cfg to the call list of the working node.
		state.current.working.add_call(called, 1)

		if __DEBUGGING__:
			write_cfg(state.current.cfg, "step", state.current.working)

		# Push the current state to the call stack with the
		# expected return address.
		state.callstack.push(state.current, type.fallthrough)

		# Update the current state with the called cfg and its entry node.
		state.current = (called, called.entry)

		if __DEBUGGING__:
			write_cfg(state.current.cfg, "step", state.current.working)

	# For return instructions, first count how many calls are stacked until it
	# reaches the correct return number.
	elif isinstance(type, ReturnType):
		pops = state.callstack.pops_count(target_addr)
		while pops > 0:
			# Connect the exit node or update count.
			state.current.cfg.add_edge(state.current.working,
								state.current.cfg.exit, 1)

			if __DEBUGGING__:
				write_cfg(state.current.cfg, "step", state.current.cfg.exit)

			# Pop the current from the call stack.
			state.current = state.callstack.pop()
			pops -= 1

	else:
		assert False, "unreachable code"

	return state

def process_program(mapping, machine):
	state = State(None, [])

	# The next group of instruction that will be executed.
	for group in machine.run():
		addr = group.leader.addr
		if not state.current:
			initial = mapping.setdefault(addr, CFG(addr))
			state.current = (initial, initial.entry)

			if __DEBUGGING__:
				write_cfg(state.current.cfg, "step", state.current.working)
		else:
			assert isinstance(state.current.working, BasicBlock)
			state = process_type(mapping, state, state.current.working.group.tail.type, addr)

		if __CACHING__:
			# Check if we processed this group from this working point.
			idx = addr % CACHE_SIZE
			cached_group, cached_working, cached_count = state.current.working.cache[idx]
			if cached_group == group:
				# And update the count.
				state.current.working.cache[idx] = (cached_group, cached_working, cached_count + 1)

				# In this case, just use the next working from the start.
				state.current.working = cached_working
			else:
				if cached_count > 0:
					state.current.cfg.flush_counts(state.current.working, cached_group, cached_working, cached_count)

				# Save the working pointer before processing the group.
				prev_working = state.current.working
				# Process the group and update working.
				state.current.working = process_group(state.current.cfg, state.current.working, group)
				# Add the new working node to the cache.
				prev_working.cache[idx] = (group, state.current.working, 0)
		else:
			state.current.working = process_group(state.current.cfg, state.current.working, group)

		if __DEBUGGING__:
			write_cfg(state.current.cfg, "step", state.current.working)

	# At the end of the machine execution, connect the working node with the halt node
	# or update the count, including all the pending current's in the call stack.
	while state.current:
		state.current.cfg.add_edge(state.current.working, state.current.cfg.halt, 1)

		if __DEBUGGING__:
			write_cfg(state.current.cfg, "step", state.current.cfg.halt)

		state.current = state.callstack.pop() if state.callstack else None

	if __CACHING__:
		# Flush all caching
		for cfg in mapping.values():
			for src in cfg.nodes:
				for (group, dst, count) in src.cache:
					if count > 0:
						cfg.flush_counts(src, group, dst, count)

	return mapping

if len(sys.argv) != 2:
	print("Usage: %s [Program Description]" % sys.argv[0])
	exit(1)

total = 1
mapping = {}
mapping = process_program(mapping, Machine(sys.argv[1]))

total = 1
for addr in mapping:
	cfg = mapping[addr]
	assert cfg.is_valid()

	write_cfg(cfg)
