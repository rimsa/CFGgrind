#!/usr/bin/env python3

from cfg import *

class HeadPoint(object):
    def __init__(self, cfg=None, dangling=None):
        self.cfg = cfg
        self.dangling = dangling

    @property
    def cfg(self):
        return self._cfg

    @cfg.setter
    def cfg(self, cfg):
        assert (not cfg) or isinstance(cfg, CFG)
        self._cfg = cfg

    @property
    def dangling(self):
        return self._dangling

    @dangling.setter
    def dangling(self, dangling):
        assert (not dangling) or isinstance(dangling, Node)
        self._dangling = dangling

class CallStack(object):
    def __init__(self):
        self._callstack = []

    def push(self, hp):
        assert isinstance(hp, HeadPoint)
        self._callstack.append(hp)

    def pop(self):
        assert self.size() > 0
        return self._callstack.pop()

    def size(self):
        return len(self._callstack)

    def __bool__(self):
        return self.size() > 0


class State(object):
    def __init__(self):
        self._current = HeadPoint()
        self._callstack = CallStack()
        self._caller = None

    @property
    def current(self):
        return self._current

    @current.setter
    def current(self, current):
        assert isinstance(current, HeadPoint)
        self._current = current

    @property
    def callstack(self):
        return self._callstack

    @property
    def caller(self):
        return self._caller

    @caller.setter
    def caller(self, caller):
        assert (not caller) or isinstance(caller, BasicBlock)
        self._caller = caller
