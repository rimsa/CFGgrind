# CFGgrind

CFGgrind is a valgrind plugin to reconstruct control flow graphs (CFGs) dynamically by following the execution of binary programs.
This tools allows successive CFGs refinements by supporting multiple executions with different inputs.
We support multi-thread programs with profiling information in the edges, calls and signal handlers.

## Building

To build CFGgrind, first download and unpack valgrind (3.15.0).

    $ wget -qO - https://sourceware.org/pub/valgrind/valgrind-3.15.0.tar.bz2 | tar jxv

Then, enter directory and clone CFGgrind github repository.
Apply the patch to add the tool in the compilation chain.

    $ cd valgrind-3.15.0
    $ git clone https://github.com/rimsa/CFGgrind.git cfggrind
    $ patch -p1 < cfggrind/cfggrind.patch

Build valgrind with CFGgrind.

    $ ./autogen.sh
    $ ./configure
    $ make -j4
    $ sudo make install

## Testing

Compile and use a test program that orders numbers given in the arguments list.
We compile it here with debugging symbols, but it is not required.

    $ cd cfggrind/tests
    $ gcc -g -ggdb -O0 -Wall -fno-stack-protector -no-pie -o test test.c
    $ ./test 15 4 8 16 42 23
    4 8 15 16 23 42

First, get the assembly instructions mapping for better CFG visualization.

    $ cfggrind_asmmap ./test > test.map
    $ head -n 5 test.map
    0x4004a8:4:sub $0x8,%rsp
    0x4004ac:7:mov 0x200b45(%rip),%rax
    0x4004b3:3:test %rax,%rax
    0x4004b6:2:je 00000000004004bd <_init+0x15>
    0x4004b8:5:callq 0000000000400540 <.plt.got>

Then, use the tool to generate an output file (test.cfg) that can be used later for CFG refinements.
Also, generate a DOT file for the bubble function (cfg-0x{addr}.dot) with the instructions loaded from the map (test.map).

    $ valgrind --tool=cfggrind --cfg-outfile=test.cfg --instrs-map=test.map --cfg-dump=bubble ./test 4 8 15 16 23 42

Generate an image from the DOT file for the bubble function.

    $ ls *.dot
    cfg-0x400627.dot
    $ dot -Tpng -o cfg-ordered.png cfg-0x400627.dot

Since the list used in the arguments was ordered, there is a phantom node for the conditional not taken inside the double loop.

<p align="center">
  <img src="tests/cfg-ordered.png?raw=true" width="540" height="750">
</p>

Use the same reference input (test.cfg) in a new execution with an unordered list as argument.
Ignore the profiling information of the previous run to account only profiling for the next execution.

    $ valgrind --tool=cfggrind --cfg-infile=test.cfg --cfg-outfile=test.cfg --instrs-map=test.map --ignore-profiling=yes --cfg-dump=bubble ./test 15 4 8 42 16 23

Update the image with the complete CFG now.

    $ dot -Tpng -o cfg-unordered.png cfg-0x400627.dot

<p align="center">
  <img src="tests/cfg-unordered.png?raw=true" width=540" heigh="1092">
</p>

## Output Format

The output format, enabled by the --cfg-outfile argument, has two main formats: **cfg** and **node**.

A **cfg** has an address (*cfg-addr*), and optionally (*curly brackets*) the number of invocations separated by
a colon (*:invocations*), if profiling is enabled at compile-time.
Also, it has a function name (*cfg-name*) in double quotes if identified in the debugging symbols,
or unknown otherwise.
Finally, it has a flag (*is-complete*) indicating if this CFG is complete -- it has no indirect jumps
or calls, and it has no phantom nodes (nodes never executed during runtime).

	[cfg cfg-addr{:invocations} cfg-name is-complete]

A **node** models a basic block with instructions, a list of function calls addresses,
a list of signals id that contains list of calls handlers, a flag indication if the last instruction of the
node is an indirect jump or call, and a list of successors nodes (node address, exit or halt).
A successor node with an address not found in the output format is a *phantom* node.
Note that there is no special entry node in the representation: there is only a single/unique node
that has the same address of its CFG, that should be executed first in case of this CFG invocation.

	[node cfg-addr node-addr node-size [list of instr-size] [list of cfg-addr{:count}]
	      [list of signal-id->cfg-addr{:count}] is-indirect [list of succ-node{:count}]]

A node must belong to a CFG (*cfg-addr*), and it is identified by its starting address (*node-addr*)
and size (*node-size*).
Then, a node has a non-empty list of instruction size (*list of instr-size*) between brackets
that must add up to *node-size*.
It has a list of CFG address with, optionally, invocation count (*list of cfg-addr{:count}*) if
the tail instruction does function calls.
Also, it has a mapping of activated signal handlers by CFG address with, optionally, invocation
count (*cfg-addr{:count}*) indexed by the signal id (*signal-id->*).
The node contains a marker if the tail instruction does an indirect jump or call (*is-indirect*).
Finally, it has a lista of successor nodes that can be a node address, exit or halt
with, optionally, executions count.

For example, the main function of file tests/signal.c can be represented as:

	[cfg 0x4005c0:1 "signal::main(11)" true]
	[node 0x4005c0 0x4005c0 38 [1 3 4 3 4 8 5 5 5] [0x4004a0:1] [] false [0x4005e6:1]]
	[node 0x4005c0 0x4005e6 10 [5 5] [0x400480:1] [] false [0x4005f0:1]]
	[node 0x4005c0 0x4005f0 2 [2] [] [] false [0x4005f7:1]]
	[node 0x4005c0 0x4005f7 10 [6 2 2] [] [14->0x4005ad:1] false [0x4005f2:56689172 0x400601:1]]
	[node 0x4005c0 0x4005f2 5 [5] [] [] false [0x4005f7:56689172]]
	[node 0x4005c0 0x400601 22 [4 3 5 5 5] [0x400470:1] [] false [0x400617:1]]
	[node 0x4005c0 0x400617 7 [5 1 1] [] [] false [exit:1]]

<p align="center">
  <img src="tests/cfg-signal.png?raw=true" width=540" heigh="1163">
</p>
