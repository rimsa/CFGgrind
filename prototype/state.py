#!/usr/bin/env python3

from cfg import *

class CurrentPair(object):
	def __init__(self, cfg = None, working = None):
		self.cfg = cfg
		self.working = working

	@property
	def cfg(self):
		return self._cfg

	@cfg.setter
	def cfg(self, cfg):
		assert (not cfg) or isinstance(cfg, CFG)
		self._cfg = cfg

	@property
	def working(self):
		return self._working

	@working.setter
	def working(self, working):
		assert (not working) or isinstance(working, Node)
		self._working = working

	def copy(self):
		return CurrentPair(self.cfg, self.working)


from collections import namedtuple
CSEntry = namedtuple('CSEntry', 'current ret_addr')

class CallStack(object):
	def __init__(self, cs = None):
		self._callstack = cs if cs else []

	def push(self, current, ret_addr):
		assert isinstance(current, CurrentPair)
		self._callstack.append(CSEntry(current, ret_addr))

	def pops_count(self, ret_addr):
		idx = self.size()
		while (idx > 0):
			idx -= 1
			if self._callstack[idx].ret_addr == ret_addr:
				return self.size() - idx
		return 0

	def pop(self):
		assert self.size() > 0
		return self._callstack.pop().current

	def size(self):
		return len(self._callstack)

	def __bool__(self):
		return self.size() > 0


class State(object):
	def __init__(self, current, cs = None):
		self.current = current
		self._callstack = CallStack(cs)

	@property
	def current(self):
		return self._current

	@current.setter
	def current(self, current):
		if not current:
			self._current = None
		elif isinstance(current, CurrentPair):
			self._current = current.copy()
		else:
			self._current = CurrentPair(current[0], current[1])

	@property
	def callstack(self):
		return self._callstack
