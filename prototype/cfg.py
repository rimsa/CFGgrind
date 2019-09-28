#!/usr/bin/env python3

from instr import *
from group import *
from config import *

from collections import namedtuple
Edge = namedtuple('Edge', 'src dst')

class Node(object):
	class Type(Enum):
		ENTRY      = 1
		BASICBLOCK = 2
		PHANTOM    = 3
		EXIT       = 4
		TERM       = 5

	def __init__(self, type):
		assert isinstance(type, Node.Type)
		self._type = type
		self._cache = [ (None, None) for _ in range(CACHE_SIZE) ]

	@property
	def type(self):
		return self._type

	@property
	def cache(self):
		return self._cache

class Entry(Node):
	def __init__(self):
		Node.__init__(self, Node.Type.ENTRY)

	def dot(self, simplified = True, padding = ""):
		str = padding + "Entry"
		if (not simplified):
			str += " [label=\"\",width=0.3,height=0.3,shape=circle,fillcolor=black,style=filled]"
		return str

class BasicBlock(Node):
	def __init__(self, group, calls = None):
		Node.__init__(self, Node.Type.BASICBLOCK)
		self._group = group
		self._calls = calls if calls else []

	@property
	def group(self):
		return self._group

	@property
	def calls(self):
		return self._calls

	@property
	def addr(self):
		return self._group.addr()

	@property
	def size(self):
		return self._group.size()

	def add_call(self, cfg):
		assert (not cfg in self._calls);
		self._calls.append(cfg)

	def add_calls(self, calls):
		self._calls.append(calls)

	def is_direct(self):
		t = self._group.tail.type
		return t.direct if hasattr(t, 'direct') else True

	def is_indirect(self):
		return not self.is_direct()

	def dot(self, simplified = True, padding = ""):
		str = padding + "\"0x%x\"" % self.addr
		if (not simplified):
			str += " [label=\"{\n"
			str += padding + "  0x%x [%d]\\l\n" % (self.addr, self.size)
			str += padding + "  | [instrs]\\l\n"
			for instr in self._group.instrs():
				str += padding + "  &nbsp;&nbsp;0x%x \<+%d\>: %s\\l\n" % \
					(instr.addr, instr.size, self._escape(instr.text))
			if (len(self._calls) > 0):
				str += padding + "  | [calls]\\l\n"
				for cfg in self._calls:
					str += padding + "  &nbsp;&nbsp;0x%x (%s)\\l\n" % (cfg.addr, cfg.name)
			str += padding + "}\"]"
		return str

	def _escape(self, str):
		return str.replace("<", "\\<").replace(">", "\\>")

	def __cmp__(self, other):
		return self.addr == other.addr if isinstance(other, BasicBlock) else False

	def __str__(self, simplified = True):
		bb = "basicblock(group: %s, calls: [" % (self._group.__str__(simplified))
		first = True
		for call in self._calls:
			if (first):
				first = False
			else:
				bb += ", "
			bb += "@0x%x" % call.addr
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


class Term(Node):
	def __init__(self):
		Node.__init__(self, Node.Type.TERM)

	def dot(self, simplified = True, padding = ""):
		str = padding + "Term"
		if (not simplified):
			str += " [label=\"\",width=0.3,height=0.3,shape=square,fillcolor=black,style=filled,peripheries=2]"
		return str

	def __str__(self):
		return "term"

class CFG(object):
	def __init__(self, addr, name = "unknown"):
		assert addr != 0
		self._addr = addr
		self._name = name
		self._entry = Entry()
		self._exit = Exit()
		self._term = Term()
		self._nodes = [self._entry, self._exit, self._term]
		self._edges = []
		self._valid = False

	@property
	def addr(self):
		return self._addr

	@property
	def name(self):
		return self._name if self._name else "unknown"

	@name.setter
	def name(self, value):
		self._name = value

	@property
	def entry(self):
		return self._entry

	@property
	def exit(self):
		return self._exit

	@property
	def term(self):
		return self._term

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

	def add_edge(self, edge):
		assert isinstance(edge, Edge)
		assert not (edge in self._edges)
		assert edge.src in self._nodes
		assert edge.dst in self._nodes
		if isinstance(edge.src, Entry):
			assert not self.succs(edge.src)
		else:
			isinstance(edge.src, BasicBlock)
		assert not isinstance(edge.dst, Entry)
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
			self.remove_edge(Edge(pred, node))
		for succ in self.succs(node):
			self.remove_edge(Edge(node, succ))
		self._nodes.remove(node)

	def remove_nodes(self, nodes):
		for node in nodes:
			self.remove_node(node)

	def remove_edge(self, edge):
		assert isinstance(edge, Edge)
		assert edge in self._edges
		self._edges.remove(edge)

	def remove_edges(self, edges):
		for edge in edges:
			self.remove_edge(edge)

	def is_valid(self):
		if (self._dirty):
			self._valid = self._check()

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
			self.add_edge(Edge(p, new))
		return new

	def split(self, node, addr):
		assert node in self._nodes
		assert isinstance(node, BasicBlock)
		assert node.group.leader.addr != addr
		assert node.group.has_instr_with_addr(addr)
		preds = self.preds(node)
		for pred in preds:
			self.remove_edge(Edge(pred, node))
		instr = node.group.pop_leader()
		new = BasicBlock(Group(instr))
		self.add_node(new)
		while node.group.leader.addr != addr:
			instr = node.group.pop_leader()
			new.group.add_instr(instr)
		for pred in preds:
			self.add_edge(Edge(pred, new))
		self.add_edge(Edge(new, node))
		return node

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

			for succ in self.succs(node):
				str += "  " + node.dot() + " -> " + succ.dot()
				if (isinstance(succ, Phantom)):
					str += " [style=dashed]"
				str += "\n"

			if (isinstance(node, BasicBlock) and node.is_indirect()):
				str += "  \"Unknown%d\" [label=\"?\", shape=none]\n" % unknown
				str += "  \"0x%x\" -> \"Unknown%d\" [style=dashed]\n" % (node.addr, unknown)
				unknown += 1

		if working:
			str += "  \"working\" [label=\"working\", fillcolor=red, fontcolor=white, style=\"rounded,filled\", shape=diamond]\n"
			str += "  \"working\" -> " + working.dot() + " [color=red]\n"

		str += "}\n"
		return str

	def _check(self):
		exit_ok = False
		term_ok = False
		for node in self._nodes:
			p = len(self.preds(node))
			s = len(self.succs(node))
			if (node == self._entry):
				assert p == 0
				if (s == 0):
					return False
			elif (node == self._exit):
				assert s == 0
				exit_ok = p > 0
			elif (node == self._term):
				assert s == 0
				exit_term = p > 0
			else:
				if (p == 0 or s == 0):
					return False

		return exit_ok or term_ok

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
		for (frm, to) in self._edges:
			if (first):
				first = False
			else:
				cfg += ", "
			cfg += "(%s, %s)" % (str(frm), str(to))
		cfg += "])"
		return cfg
