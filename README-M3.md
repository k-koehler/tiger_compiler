# Milestone 3:  Code Generation  
## Due Date: the last day for marking: April 23, 2018.    

### Read chapters  
  
*  9 (Instruction Selection),
*  10 (Liveness Analysis),
*  11 (Register Allocation)
*  12 (Putting it All Together)

in the textbook, and complete the program sections from each chapter.  For register allocation, a simple allocator for up to 27 live temporaries is sufficient for good--excellent marks.  Exceptional compilers will spill.  
___
The main.sml structure accepts additional arguments:  
*  -a generate and print assembly temporaries will be printed as T###
*  -r allocate registers all temporaries should become registers but if not, then they will still be T###  \

___ 

Next week, I will provide a syslib.s file which contains an RiscV32 system library compatible with the Gem5 simulator (https://groups.google.com/a/groups.riscv.org/forum/#!topic/sw-dev/se0TVeaA_JI)  

___ 

You may work from your Milestone 2 files, or you may use the supplied ones. As always, do the simple things first and build up.  Start with constant expressions, testing that your code makes sense, then add a larger expression and test that your code makes sense; then do single declarations, then proceed on to multiple-declaration blocks, and last recursive declarations.

___ 

Start with a register allocator that works if the function fits in caller-save registers (7 registers + 8 arguments); then extend to fit into callee-save registers (another 12 registers) where you save all of them; then extend to save only overwritten callee-save registers; then finally spill registers for functiosn that don't fit at all. 
___ 

### Info from file on moodle
* `Canon.commute :: TREE.stm * TREE.exp -> bool`
  as explained on p.175, the existing implementation works; but fails to allow most optimizations

* `RiscVGen.munchRelOp :: TREE.relop -> string`
* `RiscVGen.munchStm   :: TREE.stm -> [ Assem.instr ]`
* `RiscVGen.munchBinOp :: TREE.binop -> string`
* `RiscVGen.munchExp   :: TREE.exp -> [ Assem.instr ]`  
  currently muchRelOp and munchBinOp generate a garbage string rather than an string containing the RISCV assembly-language instruction corresponding to the op currently munchStm and munchExp call Error.impossible as a do-nothing operation

* `RiscVFrame.procEntryExit1 :: FRAME.frame * TREE.stm -> TREE.stm`
  right now, this does nothing, but as explained on p.261, it needs to pick up whatever information your register allocator figures out for the frame, and uses it to  
  * move the arguments into the temporaries where the function body expects them to be
  * move the result of a tiger function from the temporary result to the return value temporary
* `RiscVFrame.procEntryExit2 :: FRAME.frame * [ Assem.instr ] -> [ Assem.instr ]`
  right now, this does nothing, but as explained on pp. 208--208, it needs to set up the liveness properties of values needed at the end of a tiger function:
  * things like the old FP, SP, and the needed RA  
* `RiscVFrame.procEntryExit3 :: FRAME.frame * [ Assem.instr ] -> { prolog :: string, body :: [ Assem.instr ], epilog :: string }`
  right now, as described on p. 209, this emits the dummy strings identifying the start and end of the assembly-language instructions for a given tiger function ... as described on p. 261, this needs to compute assembly-language instructions that perform the view shift at the start and end of the assembly-language for a tiger function (this is the subject of most of chapter 6, specifically pp. 124--136.)

these functions will use a number of other values in RiscVFrame, things like RiscVFrame.calldefs, RiscVFrame.wordSize, RiscVFrame.externalCall, ...  

___
* You need to build the flowgraphs.  If you follow the textbook slavishly, you will put this inside a module MakeGraph structure, with the main function as documented on p. 224
  * `MakeGraph.instrs2graph :: [ Assem.instr ] -> (Flow.flowgraph, [ Graph.Node ])`
and a utility function  
  * `MakeGraph.show :: Stream * Flow.flowgraph -> unit`
  
if you want to print out your flowgraphs so you can check them.  Or, you can put that flow-graph constructor function inside the RegAlloc structure -- after all, it's only ever called from there.  Much of the rest of Liveness is done for you using the  set-based construction (the book describes two different ways to implement sets).

* Since it's an obstacle to build MakeGraph, I've uploaded the four lines into a single file, makegraph.sml along with an empty definition for instrs2graph, and added it to sources.cm. 

* You need to complete register allocation: the Color module and RegAlloc modules.  They are described on pp. 253--254 of the textbook.  You can use the Liveness implementation to see how to build programs from equational work-flow algorithms.  Register allocation is just those same algorithms, done using different equations explained on pp. 242--250. 
