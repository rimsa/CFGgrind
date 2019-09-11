#!/usr/bin/env python3

from instr import *

class Group(object):
	def __init__(self, instr, others = None):
		self._sequence = [instr]
		self._transfer = not isinstance(instr.type, StandardType)
		if others:
			self.add_instrs(others)

	@property
	def leader(self):
		return self._sequence[0]

	@property
	def tail(self):
		return self._sequence[-1]

	def addr(self):
		return self.leader.addr

	def size(self):
		s = 0
		for instr in self._sequence:
			s += instr.size
		return s

	def at(self, idx):
		return self._sequence[idx] if idx < len(self._sequence) else None

	def next(self, instr):
		idx = 0
		for i in self._sequence:
			if instr == i:
				break

			idx = idx + 1
		return self._sequence[idx + 1] if (idx + 1) < len(self._sequence) else None

	def instrs(self):
		return self._sequence.copy()

	def has_instr(self, instr):
		return instr in self._sequence

	def has_instr_with_addr(self, addr):
		for instr in self._sequence:
			if instr.addr == addr:
				return True
		return False

	def has_transfer(self):
		return self._transfer

	def add_instr(self, instr):
		assert isinstance(instr, Instruction)
		assert not self._transfer
		assert instr.addr == self.tail.addr + self.tail.size

		self._sequence.append(instr)
		if (not isinstance(instr.type, StandardType)):
			self._transfer = True

	def add_instrs(self, instrs):
		for instr in instrs:
			self.add_instr(instr)

	def pop_leader(self):
		assert len(self._sequence) > 1
		return self._sequence.pop(0)
		
	def __str__(self, simplified = True):
		grp = "["

		if (not self._sequence):
			grp = "]"
		else:
			if (not simplified):
				grp += "\n"

			first = True
			for instr in self._sequence:
				if (simplified):
					if first:
						first = False
					else:
						grp += ", "
					grp += "@0x%x" % instr.addr
				else:
					grp += "  " + str(instr) + "\n"

			grp += "]"

		return grp

	def __eq__(self, other):
		if not isinstance(other, Group):
			return False

		return set(self.instrs()) == set(other.instrs())

	def __hash__(self):
		return int(self.addr())
