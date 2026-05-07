open Sigs

module UnbalancedSet (Element : ORDERED) : SET with type elem = Element.t =
struct
  type elem = Element.t
  type tree = E | T of tree * elem * tree
  type set = tree

  let empty = E

  let rec member x t =
    match t with
    | E -> false
    | T (left, y, right) ->
        if Element.lt x y then member x left
        else if Element.lt y x then member x right
        else true

  let rec insert x s =
    match s with
    | E -> T (E, x, E)
    | T (left, y, right) ->
        if Element.lt x y then T (insert x left, y, right)
        else if Element.lt y x then T (left, y, insert x right)
        else s
end

module IntOrder : ORDERED with type t = int = struct
  type t = int

  let eq x y = x = y
  let lt x y = x < y
  let leq x y = x <= y
end
