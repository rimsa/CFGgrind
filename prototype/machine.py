#!/usr/bin/env python3

from instr import *
from group import *
import re

class Machine(object):
	def __init__(self, filename):
		self._cmds = []

		with open(filename) as fd:
			for line in [re.sub(r'#.*$', '', tmp).strip() for tmp in fd]:
				if not line:
					continue

				if line.casefold().startswith("load"):
					Machine._process_load(line)
				elif line.casefold().startswith("exec"):
					self._cmds.append(Machine._process_exec(line))
				else:
					assert False

	@staticmethod
	def _process_load(line):
		instr = re.match(r'^\s*([^\(]*)\s*\(\s*([^,]*),\s*([^,]*),\s*\"([^\"]*)\"\s*(.*)\s*\)\s*;\s*$', line)
		if instr:
			assert instr.group(1).casefold() == "load"
			addr = int(instr.group(2), 0)
			assert addr != 0
			size = int(instr.group(3))
			assert size > 0
			text = instr.group(4).strip()
			type = re.match(r'^,\s*([^\(]*)\s*\(?\s*([^\)]*)\s*\)?\s*$', instr.group(5))
			if type:
				name = type.group(1)
				tmp = type.group(2)
				args = re.split(r'\s*,\s*', tmp) if tmp else []
			else:
				assert not instr.group(5)
				name = "standard"
				args = []

			if name.casefold() == "standard":
				assert not args
				return Instruction(addr, size, text)
			elif name == "jump":
				assert len(args) >= 1 and len(args) <= 2
				target = int(args[0], 0)
				if len(args) == 2:
					if args[1] == "direct":
						direct = True
					elif args[1] == "indirect":
						direct = False
					else:
						assert False
				else:
					direct = True

				return Instruction(addr, size, text, JumpType(target, direct))
			elif name.casefold() == "branch":
				assert len(args) >= 2 and len(args) <= 3

				target = int(args[0], 0)
				fallthrough = int(args[1], 0)
				assert fallthrough != 0

				if len(args) == 3:
					if args[2] == "direct":
						direct = True
					elif args[2] == "indirect":
						direct = False
					else:
						assert False
				else:
					direct = True

				return Instruction(addr, size, text, BranchType(target, fallthrough, direct))
			elif name == "call":
				assert len(args) >= 2 and len(args) <= 3
				target = int(args[0], 0)
				fallthrough = int(args[1], 0)
				assert fallthrough != 0

				if len(args) == 3:
					if args[2] == "direct":
						direct = True
					elif args[2] == "indirect":
						direct = False
					else:
						assert False
				else:
					direct = True

				return Instruction(addr, size, text, CallType(target, fallthrough, direct))
			elif name == "return":
                                assert not args
                                return Instruction(addr, size, text, ReturnType())
			else:
				assert False

	@staticmethod
	def _process_exec(line):
		obj = re.match(r'^\s*([^\(]*)\s*\(\s*(.*)\s*\)\s*;\s*$', line)
		if obj:
			assert obj.group(1).casefold() == "exec"
			return int(obj.group(2), 0)
		else:
			assert False

	def _find_group(self, addr):
		instr = Instruction.find(addr)
		assert instr

		group = Group(instr)
		while isinstance(instr.type, StandardType):
			instr = Instruction.find(instr.addr + instr.size)
			if not instr:
				break

			group.add_instr(instr)
		return group

	def start_addr(self):
		return self._cmds[0]

	def run(self):
		return list(map(lambda x: self._find_group(x), self._cmds))
