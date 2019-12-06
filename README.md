# CFGgrind

CFGgrind is a valgrind plugin to reconstruct control flow graphs (CFGs) dynamically by following the execution of binary programs.
This tools allows successive CFGs refinements by supporting multiple executions with different inputs.
We use profiling for edge counts and multi-thread programs.

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
