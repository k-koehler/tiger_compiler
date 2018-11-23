signature COLOR =
sig
  structure Frame : FRAME

  type allocation = Frame.register Temp.Table.table

  val color : { interference : Liveness.igraph
              ,      initial : allocation
              ,    spillCost : Graph.node -> int 
              ,    registers : Frame.register list
              } -> allocation * Temp.temp list
end

structure Color : COLOR =
struct
  structure Frame = RiscVFrame

  structure A = Assem
  structure E = ErrorMsg
  structure F = RiscVFrame
  structure G = Graph
  structure M = Temp
  (* set of ints  *)
  structure Set = RedBlackSetFn(struct
                                  type ord_key = int
                                  val compare = Int.compare
                                end)

  (* set of tuples (int, int) *)
  structure TS = RedBlackSetFn(struct
                                type ord_key = int * int
                                val compare = 
                                  fn ((x1, y1), (x2, y2)) => 
                                    if Int.compare (x1, x2) = EQUAL 
                                    then Int.compare (y1, y2) 
                                    else Int.compare (x1, x2) (* just return order based on x *)
                              end)

  fun nodeToInt n = G.getNodeValue n (* convert a node to an int *)
  fun nodesToInt n = map nodeToInt n (* convert a list of nodes to ints *)

  fun append list' item = list' @ [item]

  fun toSet someStack = Set.empty

  fun quickSet n = Set.union(Set.empty, n)

  type allocation = F.register M.Table.table
  
  (* add an item to the end of a list *)

  fun color ({interference, initial, spillCost, registers}) = 
    let
      (* node worklists, sets, stacks *)
      val precoloured = ref Set.empty (* predefined *)
      val initial = ref Set.empty
      val simplifyWorklist = ref Set.empty
      val freezeWorklist = ref Set.empty
      val spillWorklist = ref Set.empty
      val spilledNodes = ref Set.empty
      val coalescedNodes = ref Set.empty
      val coloredNodes = ref Set.empty
      val selectStack = ref Stack.empty (* whenever you use this, you MUST return a new value *)
      (*move sets*)
      val coalescedMoves = ref Set.empty
      val constrainedMoves = ref Set.empty
      val frozenMoves = ref Set.empty
      val worklistMoves = ref Set.empty
      val activeMoves = ref Set.empty
      (*other data structures*)
      val adjSet = ref TS.empty
      val adjList = Array.array(1000, Set.empty)
      val degree = Array.array(1000, 0)
      val moveList = Array.array(1000, Set.empty)
      val colour = Array.array(1000, 0)
      val alias = Array.array(1000,0)
      val nodeMoves = Array.array(1000, Set.empty)
      fun colorOfNode node = node

      val K = 32 (*set real val*)

      fun AddEdge(u, v) = 
        let
          val u = nodeToInt u
          val v = nodeToInt v
          val _ =
            if not(TS.member(!adjSet, (u, v))) andalso (u <> v) then
                 adjSet := TS.addList(!adjSet, [(u, v), (v, u)])
            else ()
          val _ = 
            if Set.member(!precoloured, u) then
              Array.update(adjList, u, Set.add(Array.sub(adjList, u), v))
            else if Set.member(!precoloured, v) then
              Array.update(adjList, v, Set.add(Array.sub(adjList, v), u))
            else ()
          val _ = 
            if Set.member(!precoloured, u) then
              Array.update(degree, u, Array.sub(degree, u) + 1)
            else if Set.member(!precoloured, v) then
              Array.update(degree, v, Array.sub(degree, v) + 1)
            else ()
          in
            ()
          end
      fun Adjacent(n) =
        Set.difference(Array.sub(adjList, n), Set.union(toSet (!selectStack), !coalescedNodes))
        
      fun NodeMoves(n) =
        Set.intersection(Array.sub(moveList, n), Set.union(!activeMoves, !worklistMoves))
        
      fun MoveRelated(n) = 
        not(Set.isEmpty(NodeMoves(n)))

      fun MakeWorklist() =
          let
            fun processTemp n = 
              let
                val _ =
                  if Array.sub(degree, n) >= K then
                    spillWorklist := Set.union(!spillWorklist, Set.add(Set.empty, n))
                  else ()
                val _ =
                  if Array.sub(degree, n) < K andalso MoveRelated(n) then
                    freezeWorklist := Set.union(!freezeWorklist, Set.add(Set.empty, n))
                  else ()
                val _ =
                  if Array.sub(degree, n) < K andalso MoveRelated(n) = false then
                    simplifyWorklist := Set.union(!simplifyWorklist, Set.add(Set.empty, n))
                  else ()
              in 
                ()
              end
          in
            (map processTemp (Set.listItems(!initial)); initial := Set.empty)
          end


      fun EnableMoves(nodes) =
        let
          fun aux m = 
            (activeMoves := Set.difference(!activeMoves, Set.add(Set.empty, m));
             worklistMoves := Set.union(!worklistMoves, Set.add(Set.empty, m)))
        in
          map( fn n => map( fn m => aux m))
        end

      fun DecrementDegree(m) =
        let
          val d = Array.sub(degree, m)
          val _ = Array.update(degree, m, Array.sub(degree, m) - 1)
          val _ = 
            if d = K then
              (EnableMoves(Set.union(Set.add(Set.empty, m), Adjacent(m)));
              spillWorklist := Set.difference(!spillWorklist, Set.add(Set.empty, m));
              if MoveRelated(m) then
                freezeWorklist := Set.union(!freezeWorklist, Set.add(Set.empty,  m))
              else 
                simplifyWorklist := Set.union(!simplifyWorklist, Set.add(Set.empty, m)))
            else
              ()
          in
            ()
          end

      fun AddWorkList(n) =
        let
          val _ = 
            if (not(Set.member(!precoloured, n)) andalso not(MoveRelated(n))) andalso Array.sub(degree, n) < K then
              (freezeWorklist := Set.difference(!freezeWorklist, Set.add(Set.empty, n));
               simplifyWorklist := Set.union(!simplifyWorklist, Set.add(Set.empty, n)))
            else 
              ()
          in
            ()
          end

      fun OK (t, r) = Array.sub(degree, t) < K orelse Set.member(!precoloured, t) orelse TS.member(!adjSet, (t, r)) 

      fun Simplify() = 
        let
          (*TODO non-arbitrary n*)
          val n = hd(Set.listItems(!simplifyWorklist))
          val _ = Set.difference(!simplifyWorklist, Set.add(Set.empty, n))
          val _ = selectStack := Stack.push(n, !selectStack)
          val _ = app (fn m => DecrementDegree(m)) (Set.listItems(Adjacent(n)))
        in
          ()
        end

      fun Conservative(nodes) =
        let
          val k = ref 0
          val _ = app (fn n => if Array.sub(degree, n) >= K then k := !k + 1 else ()) nodes 
        in 
          (!k < K)
        end

      fun GetAlias(n) = if Set.member(!coalescedNodes, n) then GetAlias(Array.sub(alias, n)) else n

      fun Combine(u, v) = 
        let
          val _ = 
            if Set.member(!freezeWorklist, v) 
            then freezeWorklist := Set.difference(!freezeWorklist, Set.add(Set.empty, v))
            else spillWorklist := Set.difference(!spillWorklist, Set.add(Set.empty, v))
          val _ = 
            (
                coalescedNodes := Set.union(!coalescedNodes, Set.add(Set.empty, v));
                Array.update(alias, v, u);
                Array.update(nodeMoves, u, Set.union(Array.sub(nodeMoves, u), Array.sub(nodeMoves,v)))
             )
          (*val _ =
            map(fn t => (AddEdge(t,u); DecrementDegree(t); t), Adjacent(v)) FIX THIS *)
        (*TODO complete this*)
        in
          ()
        end

      fun Coalesce() =
        let
          val m = hd (Set.listItems (!worklistMoves))
          val (x, y) = (~1, ~1) (* We need to look through th graph and find what the are moves src and dest *)
          val x = GetAlias(x)
          val y = GetAlias(y)
          val (u, v) =
            if Set.member(!precoloured, y) then
              (y, x)
            else 
              (x, y)
          val _ = worklistMoves := Set.difference(!worklistMoves, Set.add(Set.empty, m))
          val _ = 
            if u = v then
              (AddWorkList(u);
              (coalescedMoves := Set.union(!coalescedMoves, Set.add(Set.empty, m))))
            else if Set.member(!precoloured, v) orelse TS.member(!adjSet, (u,v)) then
              (AddWorkList(u);
              AddWorkList(v);
              (constrainedMoves := Set.union(!constrainedMoves, Set.add(Set.empty, m))))
            else if 
              (Set.member(!precoloured, u) andalso 
              not(List.exists (fn x => x = false) (List.map (fn t => OK(t, u)) (Set.listItems(Adjacent(v))))) orelse 
              (not (Set.member(!precoloured, u)) andalso 
              Conservative(Set.listItems (Set.union(Adjacent(u), Adjacent(v)))))) then
              (Combine(u,v);
              AddWorkList(u);
              coalescedMoves := Set.union(!coalescedMoves, Set.add(Set.empty, m)))
            else
              activeMoves := Set.union(!activeMoves, Set.add(Set.empty, m))
        in 
          ()
        end
        
      fun FreezeMoves(n) = ()
      
      fun Freeze() =
        let 
            val u =
                hd (Set.listItems(!freezeWorklist))
            val _ =
                (
                    freezeWorklist := Set.difference(!freezeWorklist, Set.add(Set.empty, u));
                    simplifyWorklist := Set.union(!simplifyWorklist, Set.add(Set.empty, u));
                    FreezeMoves(u)
                )
        in
            ()
        end
        
      fun SelectSpill() =
        let
            val m = hd (Set.listItems(!spillWorklist)) (*TODO heuristic*)
            val _ =
            (
                spillWorklist := Set.difference(!spillWorklist,  Set.add(Set.empty, m));
                simplifyWorklist := Set.union(!simplifyWorklist, Set.add(Set.empty, m));
                FreezeMoves(m)
            )
        in
            ()
        end
      
      fun AssignColoursAux(n) =
        let
            val okColours = 
                ref (Set.addList(Set.empty, List.tabulate(K-1, fn x => x+1)))
            fun helper w = 
              if Set.member(Set.union(!coloredNodes, !precoloured), GetAlias(w)) 
              then okColours := Set.difference(!okColours, Set.add(Set.empty, (Array.sub(colour, GetAlias(w)))))
              else ()
            val _ =
                Set.app helper (Array.sub(adjList, n))
            val _ = 
                if Set.isEmpty(!okColours) then
                    spilledNodes := Set.union(!spilledNodes, Set.add(Set.empty,n))
                else 
                (
                    coloredNodes := Set.union(!coloredNodes, Set.add(Set.empty,n));
                    Array.update(colour, n, hd (Set.listItems(!okColours))) 
                )
            val _ =
                Set.map (fn n => (Array.update(colour, n, Array.sub(colour, GetAlias(n))); n)) (!coalescedNodes)
        in
            ()
        end
      
      fun AssignColors() =
        let
            val n = 
                SOME(Stack.top(!selectStack)) handle _ => NONE
            val _ = 
                selectStack := Stack.pop(!selectStack)
            val _ =
                case n of SOME value => (AssignColoursAux(value); AssignColors())
                        | NONE => ()
        in
            ()
        end
    
    fun Build() = ()
    
    fun MainAux() =
        let
            val _ =
                if not(Set.isEmpty(!simplifyWorklist)) then Simplify() 
                else if not(Set.isEmpty(!worklistMoves)) then Coalesce() 
                else if not(Set.isEmpty(!freezeWorklist)) then Freeze() 
                else if not(Set.isEmpty(!spillWorklist)) then SelectSpill() 
                else ()
        in
            ()
        end
        
    fun Main() =
        let
            val _ =
                Build()
            val repeatCond =
                Set.isEmpty(!simplifyWorklist)  andalso 
                Set.isEmpty(!worklistMoves)     andalso 
                Set.isEmpty(!freezeWorklist)    andalso 
                Set.isEmpty(!spillWorklist) 
            val _ =
                while not(repeatCond) do
                    MainAux()
            val _ =
                AssignColors()
            val _ =
                if not(Set.isEmpty(!spilledNodes)) then
                (
                    (*RewriteProgram();*)
                    Main()
                )
                else
                    ()
        in
            ()
        end

    in
      (M.Table.empty,  [])
    end

end
