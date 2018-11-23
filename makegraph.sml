structure MakeGraph :
  sig
    val instrs2graph : Assem.instr list -> Flow.flowgraph * Graph.node list
  end

= struct
  structure E = ErrorMsg
  structure W = Flow
  structure G = Graph
  structure S = Symbol
  structure A = Assem
  
  fun instrs2graph instrslist = 
    let 
      val graph = Graph.newGraph()
      val nodes = ref []

      val labelToNode = ref S.empty (* needs to be a special type so we can have string keys *)

      fun makeAndAddNodes (instrTable, useTable, defTable, isMoveTable, nil) = (instrTable, useTable, defTable, isMoveTable)
        | makeAndAddNodes (instrTable, useTable, defTable, isMoveTable, instruction::rest) = 
          let
            val node = Graph.newNode(graph)
            (* using `new` here for the lack of a better term *)
            val (newDef, newUse, newIsMove) = 
              case instruction 
                of A.OPER {assem, dst, src, jump} => (dst, src, false)
                | A.LABEL {assem, lab} => (* we need to handle this case, to help with edges *)
                    (labelToNode := S.enter(!labelToNode, lab, node);
                    ([], [], false))
                | A.MOVE {assem, src, dst} => ([dst], [src], true)
            (* why do we have to return new values *)
            val newInstrTable = Graph.Table.enter(instrTable, node, instruction)
            val newUseTable = Graph.Table.enter(useTable, node, newUse)
            val newDefTable = Graph.Table.enter(defTable, node, newDef)
            val newIsMoveTable = Graph.Table.enter(isMoveTable, node, newIsMove)
            val _ = ((nodes := !nodes @ [node]);()) (* append the node to the list of nodes *)
          in
            makeAndAddNodes(newInstrTable, newUseTable, newDefTable, newIsMoveTable, rest)
          end

      (* this is the first pass to add the nodes only *)
      val (instrTable, useTable, defTable, isMoveTable) = 
        makeAndAddNodes(Graph.Table.empty, Graph.Table.empty, Graph.Table.empty, Graph.Table.empty, instrslist)
      
      fun connectNodes (nil) = ()
        | connectNodes (node::nil) = ()
        | connectNodes (node::nextNode::rest) = 
        let
          val SOME(nodeInstr) = Graph.Table.look(instrTable, node)
          val jumpLabels = 
            case nodeInstr
              of A.OPER{jump=SOME(labels), ...} => labels
               | _ => []
          val nodesToLinkTo = 
            if List.null jumpLabels (* if there are no jump lables *)
            then [nextNode] (* we will connect to the next node *)
            else List.mapPartial (fn lbl => S.look(!labelToNode, lbl)) jumpLabels (* extract the node from the labelToNode map *)
          val _ = map (fn x => Graph.mk_edge {from=node, to=x}) nodesToLinkTo
        in
          (connectNodes([nextNode] @ rest); ())
        end

      (* this is the second pass, we can now add the edges of the nodes *)
      val _ = connectNodes(!nodes)

    in
      (W.FGRAPH{control=graph, use=useTable, def=defTable, ismove=isMoveTable}, !nodes)
    end 
end
