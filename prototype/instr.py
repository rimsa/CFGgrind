#!/usr/bin/env python3

from enum import Enum

class Type(object):
	class Id(Enum):
		STANDARD = 1
		JUMP     = 2
		BRANCH   = 3
		CALL     = 4
		RETURN   = 5

	def __init__(self, type):
		assert isinstance(type, Type.Id)
		self._type = type

	@property
	def type(self):
		return self._type

class StandardType(Type):
	def __init__(self):
		Type.__init__(self, Type.Id.STANDARD)

	def __str__(self):
		return "standard"

class JumpType(Type):
	def __init__(self, target, direct = True):
		if (target == 0):
			assert not direct
		else:
			assert direct

		Type.__init__(self, Type.Id.JUMP)
		self._target = target
		self._direct = direct

	@property
	def target(self):
		return self._target

	@property
	def direct(self):
		return self._direct

	@property
	def indirect(self):
		return not self._direct

	def __str__(self):
		return "jump(@0x%x, %s)" % (self.target, ("direct" if self.direct else "indirect"))

class BranchType(Type):
	def __init__(self, target, fallthrough, direct = True):
		if (target == 0):
			assert not direct
		else:
			assert direct
		assert fallthrough != 0

		Type.__init__(self, Type.Id.BRANCH)
		self._target = target
		self._fallthrough = fallthrough
		self._direct = direct

	@property
	def target(self):
		return self._target

	@property
	def fallthrough(self):
		return self._fallthrough

	@property
	def direct(self):
		return self._direct

	@property
	def indirect(self):
		return not self._direct

	def __str__(self):
		return "branch(@0x%x, @0x%x, %s)" % (self.target, self.fallthrough, ("direct" if self.direct else "indirect"))

class CallType(Type):
	def __init__(self, target, fallthrough, direct = True):
		if (target == 0):
			assert not direct
		else:
			assert direct

		Type.__init__(self, Type.Id.CALL)
		self._target = target
		self._fallthrough = fallthrough
		self._direct = direct

	@property
	def target(self):
		return self._target

	@property
	def fallthrough(self):
		return self._fallthrough

	@property
	def direct(self):
		return self._direct

	@property
	def indirect(self):
		return not self._direct

	def __str__(self):
		return "call(@0x%x, @0x%x, %s)" % (self.target, self.fallthrough,
					("direct" if self.direct else "indirect"))

class ReturnType(Type):
	def __init__(self):
		Type.__init__(self, Type.Id.RETURN)

	def __str__(self):
		return "return"

class Instruction(object):
	_all = {}

	def __new__(cls, *args, **kwargs):
		addr = args[0]
		assert addr != 0
		assert not (addr in cls._all)

		instance = object.__new__(cls)
		cls._all[addr] = instance
		return instance

	def __del__(self):
		if (self._addr in Instruction._all):
			del self._all[self._addr]

	@classmethod
	def find(cls, addr):
		if (addr in cls._all):
			return cls._all[addr]

	@classmethod
	def all(cls):
		return cls._all.values()

	def __init__(self, addr, size, text = '', type = None):
		assert addr != 0 and size != 0
		if not type:
			type = StandardType()
		assert isinstance(type, Type)
		self._addr = addr
		self._size = size
		self._text = text
		self._type = type

	@property
	def addr(self):
		return self._addr

	@property
	def size(self):
		return self._size

	@property
	def text(self):
		return self._text

	@property
	def type(self):
		return self._type

	def __str__(self):
		return "(@0x%x, %d, %s, \"%s\")" % (self.addr, self.size, self.type, self.text)
