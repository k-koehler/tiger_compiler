(* References http://www.it.uu.se/edu/course/homepage/funpro/ht07/slides/f09-modules.4up.pdf see slide 8 *)

signature AbsStack =
sig
  exception EmptyStack
  type 'item stack
  val empty : 'item stack
  val pop : 'item stack -> 'item stack
  val push : 'item * 'item stack -> 'item stack
  val top : 'item stack -> 'item
end

structure Stack :> AbsStack =
struct
  exception EmptyStack

  datatype 'item stack = Empty
                       | Node of 'item * 'item stack
  val empty = Empty

  fun pop (Empty) = raise EmptyStack
    | pop (Node(item, stack)) = stack

  fun push (item, stack) = Node(item, stack)

  fun top (Empty) = raise EmptyStack
    | top (Node(item, stack)) = item

end