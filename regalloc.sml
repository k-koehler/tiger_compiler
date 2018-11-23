signature REG_ALLOC =
sig
  structure Frame : FRAME
  type allocation = Frame.register Temp.Table.table
  val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation
end

structure RegAlloc : REG_ALLOC =
struct
  structure Frame = RiscVFrame

  structure A = Assem
  structure C = Color
  structure E = ErrorMsg
  structure G = MakeGraph
  structure L = Liveness
  structure M = Temp

  type allocation = Frame.register M.Table.table

  fun alloc (instrs, frame) = 
    let 
      val (flowGraph, nodes) = G.instrs2graph(instrs)
      val (igraph, liveOut) = L.interferenceGraph(flowGraph, nodes)
      (* val _ = map print (map Int.toString (map Graph.getNodeValue nodes)) *) (* testing the node value of the graph *)
      (* val _ = L.show(TextIO.stdOut, igraph) *) (* use this to print the liveness *)
      val (alloc, spills) = 
        C.color({interference = igraph, initial = Frame.tempMap, spillCost = (fn x => 1), registers = Frame.registers})
    in
      (instrs, alloc)
    end
      
end
