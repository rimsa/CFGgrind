#!/usr/bin/env python3

from instr import *
from group import *
from config import *

class Node(object):
	class Type(Enum):
		ENTRY      = 1
		BASICBLOCK = 2
		PHANTOM    = 3
		EXIT       = 4
		HALT       = 5

	def __init__(self, type):
		assert isinstance(type, Node.Type)
		self._type = type
		self._cache = [ (None, None, 0) for _ in range(CACHE_SIZE) ]

	@property
	def type(self):
		return self._type

	@property
	def cache(self):
		return self._cache

class Edge(object):
	def __init__(self, src, dst, count):
		self._src = src
		self._dst = dst
		self._count = count

	@property
	def src(self):
		return self._src

	@property
	def dst(self):
		return self._dst

	@dst.setter
	def dst(self, dst):
		self._dst = dst

	@property
	def count(self):
		return self._count

	@count.setter
	def count(self, count):
		self._count = count

	def updateCount(self, count):
		assert count >= 0
		self._count += count

class Entry(Node):
	def __init__(self):
		Node.__init__(self, Node.Type.ENTRY)

	def dot(self, simplified = True, padding = ""):
		str = padding + "Entry"
		if (not simplified):
			str += " [label=\"\",width=0.3,height=0.3,shape=circle,fillcolor=black,style=filled]"
		return str

	def __str__(self):
		return "entry"

class BasicBlock(Node):
	def __init__(self, group, calls = None, signals = None):
		Node.__init__(self, Node.Type.BASICBLOCK)
		self._group = group
		self._calls = calls if calls else {}
		self._signals = signals if signals else {}

	@property
	def group(self):
		return self._group

	@property
	def calls(self):
		return self._calls

	@property
	def signals(self):
		return self._signals

	@property
	def addr(self):
		return self._group.addr()

	@property
	def size(self):
		return self._group.size()

	def add_call(self, cfg, count):
		if cfg.addr in self._calls:
			_, prev_count = self._calls[cfg.addr]
			self._calls[cfg.addr] = (cfg, prev_count + count)
		else:
			self._calls[cfg.addr] = (cfg, count)

	def add_signal(self, sigid, cfg, count):
		if sigid in self._signals:
			prev_cfg, prev_count = self._signals[sigid]
			assert cfg.addr == prev_cfg.addr
			self._signals[sigid] = (cfg, prev_count + count)
		else:
			self._signals[sigid] = (cfg, count)

	def is_direct(self):
		t = self._group.tail.type
		return t.direct if hasattr(t, 'direct') else True

	def is_indirect(self):
		return not self.is_direct()

	def dot(self, simplified = True, padding = ""):
		str = padding + "\"0x%x\"" % self.addr
		if not simplified:
			str += " [label=\"{\n"
			str += padding + "  0x%x [%d]\\l\n" % (self.addr, self.size)
			str += padding + "  | [instrs]\\l\n"
			for instr in self._group.instrs():
				str += padding + "  &nbsp;&nbsp;0x%x \<+%d\>: %s\\l\n" % \
					(instr.addr, instr.size, self._escape(instr.text))
			if (len(self._calls) > 0):
				str += padding + "  | [calls]\\l\n"
				for addr in self._calls:
					cfg, count = self._calls[addr]
					str += padding + "  &nbsp;&nbsp;0x%x \{%d\} (%s)\\l\n" % (addr, count, cfg.name)
			if (len(self._signals) > 0):
				str += padding + "  | [signals]\\l\n"
				for sigid in self._signals:
					cfg, count = self._signals[sigid]
					str += padding + "  &nbsp;&nbsp;%d: 0x%x \{%d\} (%s)\\l\n" % (sigid, cfg.addr, count, cfg.name)
			str += padding + "}\"]"
		return str

	def _escape(self, str):
		return str.replace("<", "\\<").replace(">", "\\>")

	def __cmp__(self, other):
		return self.addr == other.addr if isinstance(other, BasicBlock) else False

	def __str__(self, simplified = True):
		bb = "basicblock(group: %s, calls: [" % (self._group.__str__(simplified))
		first = True
		for addr in self._calls:
			if (first):
				first = False
			else:
				bb += ", "
			bb += "@0x%x" % addr
			if not simplified:
				_, count = self._calls[addr]
				bb += "{%d}" % count
		bb += "], signals: ["
		for sigid in self._signals:
			cfg, count = self._signals[sigid]
			if (first):
				first = False
			else:
				bb += ", "
			bb += "@0x%x" % cfg.addr
			if not simplified:
				bb += "{%d}" % count
		bb += "])"
		return bb

class Phantom(Node):
	def __init__(self, addr):
		Node.__init__(self, Node.Type.PHANTOM)
		self._addr = addr


	@property
	def addr(self):
		return self._addr

	def dot(self, simplified = True, padding = ""):
		str = padding + "\"0x%x\"" % self.addr
		if (not simplified):
			str += " [label=\"0x%lx\", style=dashed]" % self.addr
		return str

	def __cmp__(self, other):
		return self.addr == other.addr if isinstance(other, Phantom) else False

	def __str__(self):
		return "phantom(@0x%x)" % self._addr

class Exit(Node):
	def __init__(self):
		Node.__init__(self, Node.Type.EXIT)

	def dot(self, simplified = True, padding = ""):
		str = padding + "Exit"
		if (not simplified):
			str += " [label=\"\",width=0.3,height=0.3,shape=circle,fillcolor=black,style=filled,peripheries=2]"
		return str

	def __str__(self):
		return "exit"


class Halt(Node):
	def __init__(self):
		Node.__init__(self, Node.Type.HALT)

	def dot(self, simplified = True, padding = ""):
		str = padding + "Halt"
		if (not simplified):
			str += " [label=\"\",width=0.3,height=0.3,shape=square,fillcolor=black,style=filled,peripheries=2]"
		return str

	def __str__(self):
		return "halt"

class CFG(object):
	def __init__(self, addr, name = "unknown"):
		assert addr != 0
		self._addr = addr
		self._name = name
		self._entry = Entry()
		self._exit = Exit()
		self._halt = Halt()
		self._nodes = [self._entry, self._exit, self._halt]
		self._edges = []
		self._valid = False

	@property
	def addr(self):
		return self._addr

	@property
	def name(self):
		return self._name if self._name else "unknown"

	@name.setter
	def name(self, name):
		self._name = name

	@property
	def entry(self):
		return self._entry

	@property
	def exit(self):
		return self._exit

	@property
	def halt(self):
		return self._halt

	@property
	def nodes(self):
		return self._nodes.copy()

	@property
	def edges(self):
		return self._edges.copy()

	def addr2node(self, addr):
		for node in self.nodes:
			if (isinstance(node, BasicBlock) or isinstance(node, Phantom)):
				if (node.addr == addr):
					return node

	def add_node(self, node):
		assert isinstance(node, BasicBlock) or isinstance(node, Phantom)
		assert not self.addr2node(node.addr)
		self._nodes.append(node)
		self._dirty = True
		return node

	def _find_edge(self, src, dst):
		for edge in self._edges:
			if edge.src == src and edge.dst == dst:
				return edge
		return None

	def add_edge(self, src, dst, count):
		assert count >= 0
		edge = self._find_edge(src, dst)
		if edge:
			edge.updateCount(count)
		else:
			assert src in self._nodes
			assert dst in self._nodes
			if isinstance(src, Entry):
				assert not self.succs(src)
			else:
				isinstance(src, BasicBlock)
			assert not isinstance(dst, Entry)
			edge = Edge(src, dst, count)
			self._edges.append(edge)
		self._dirty = True
		return edge.dst

	def succs(self, node):
		return [ edge.dst for edge in self._edges if edge.src == node ]

	def preds(self, node):
		return [ edge.src for edge in self._edges if edge.dst == node ]

	def remove_node(self, node):
		assert node in self._nodes
		for pred in self.preds(node):
			self.remove_edge(pred, node)
		for succ in self.succs(node):
			self.remove_edge(node, succ)
		self._nodes.remove(node)

	def remove_edge(self, src, dst):
		edge = self._find_edge(src, dst)
		assert edge
		self._edges.remove(edge)

	def is_valid(self):
		if self._dirty:
			self._valid = self._check()
			self._dirty = False

		return self._valid

	def find_node_with_addr(self, addr):
		for node in self.nodes:
			if isinstance(node, BasicBlock):
				for instr in node.group.instrs():
					if instr.addr == addr:
						return node
			elif isinstance(node, Phantom):
				if node.addr == addr:
					return node
		return None

	def phantom2basicblock(self, old, new):
		assert old in self._nodes
		assert isinstance(old, Phantom)
		assert not new in self._nodes
		assert isinstance(new, BasicBlock)
		preds = self.preds(old)
		self.remove_node(old)
		self.add_node(new)
		for p in preds:
			self.add_edge(p, new, 0)
		return new

	def split(self, node, addr):
		assert node in self._nodes
		assert isinstance(node, BasicBlock)
		assert node.group.leader.addr != addr
		assert node.group.has_instr_with_addr(addr)

		instr = node.group.pop_leader()
		new = BasicBlock(Group(instr))
		self.add_node(new)
		while node.group.leader.addr != addr:
			instr = node.group.pop_leader()
			new.group.add_instr(instr)

		count = 0
		for pred in self.preds(node):
			edge = self._find_edge(pred, node)
			assert edge
			count += edge.count
			edge.dst = new

		self.add_edge(new, node, count)
		return node

	def flush_counts(self, src, group, dst, count):
		assert count > 0
		size = 0
		while size < group.size():
			tmp = self.addr2node(group.leader.addr + size)
			assert tmp

			edge = self._find_edge(src, tmp)
			assert edge
			edge.updateCount(count)

			size += tmp.size
			src = tmp
		assert size == group.size()
		assert src == dst
		self._dirty = True

	def _check(self):
		has_exit = False
		has_halt = False
		for node in self._nodes:
			preds_total = preds_count = 0
			for pred in self.preds(node):
				edge = self._find_edge(pred, node)
				preds_count += edge.count
				preds_total += 1

			succs_total = succs_count = 0
			for succ in self.succs(node):
				edge = self._find_edge(node, succ)
				succs_count += edge.count
				succs_total += 1

			if (node == self._entry):
				if preds_total != 0 or succs_total == 0 or succs_count == 0:
					return False
			elif (node == self._exit):
				if preds_total > 0:
					if succs_total != 0 or preds_count == 0:
						return False

					has_exit = True
			elif (node == self._halt):
				if preds_total > 0:
					if succs_total != 0 or preds_count == 0:
						return False

					has_halt = True
			else:
				if preds_total == 0 or succs_total == 0 or preds_count != succs_count:
					return False

		return has_exit or has_halt

	def dot(self, working = None):
		str = "digraph \"0x%x\" {\n" % self._addr
		str += "  label = \"0x%x (%s)\"\n" % (self._addr, self._name)
		str += "  labelloc = \"t\"\n"
		str += "  viewport = \"1080,1440,1\"\n"
		str += "  node[shape=record]\n\n"

		unknown = 1
		for node in self._nodes:
			if (node == self.entry or len(self.preds(node))):
				str += node.dot(False, "  ") + "\n"

			if (isinstance(node, BasicBlock) and node.is_indirect()):
				str += "  \"Unknown%d\" [label=\"?\", shape=none]\n" % unknown
				str += "  \"0x%x\" -> \"Unknown%d\" [style=dashed]\n" % (node.addr, unknown)
				unknown += 1

		for edge in self._edges:
			str += "  " + edge.src.dot() + " -> " + edge.dst.dot()
			if isinstance(edge.dst, Phantom):
				assert edge.count == 0
				str += " [style=dashed]"
			elif edge.count > 0:
				str += " [label=\"%d\"]" % edge.count
			str += "\n"

		if working:
			str += "  \"working\" [label=\"working\", fillcolor=red, fontcolor=white, style=\"rounded,filled\", shape=diamond]\n"
			str += "  \"working\" -> " + working.dot() + " [color=red]\n"

		str += "}\n"
		return str

	def __str__(self):
		cfg = "(["
		first = True
		for node in self.nodes:
			if (first):
				first = False
			else:
				cfg += ", "
			cfg += str(node)
		cfg += "], ["
		first = True
		for edge in self._edges:
			if (first):
				first = False
			else:
				cfg += ", "
			cfg += "(%s, %s, %d)" % (edge.src, edge.dst, edge.count)
		cfg += "])"
		return cfg
