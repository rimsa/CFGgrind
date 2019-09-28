#!/usr/bin/env python3

from cfg import *

class CurrentPair(object):
	def __init__(self, cfg=None, working=None):
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


class CallStack(object):
	def __init__(self, cs = []):
		self._callstack = cs

	def push(self, hp):
		assert isinstance(hp, CurrentPair)
		self._callstack.append(hp.copy())

	def pop(self):
		assert self.size() > 0
		return self._callstack.pop()

	def size(self):
		return len(self._callstack)

	def __bool__(self):
		return self.size() > 0


class State(object):
	def __init__(self, current, cs = [], pending = None):
		self.current = current
		self._callstack = CallStack(cs)
		self._pending = pending

	@property
	def current(self):
		return self._current

	@current.setter
	def current(self, current):
		if (isinstance(current, CurrentPair)):
			self._current = current.copy();
		else:
			self._current = CurrentPair(current[0], current[1]);

	@property
	def callstack(self):
		return self._callstack

	@property
	def pending(self):
		return self._pending

	@pending.setter
	def pending(self, pending):
		assert (not pending) or isinstance(pending, BasicBlock)
		self._pending = pending
