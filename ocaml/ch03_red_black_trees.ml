open Sigs

module RedBlackSet (Element : ORDERED) = struct
  type elem = Element.t
  type color = R | B
  type tree = E | T of color * tree * elem * tree
  type set = tree

  let empty = E

  let rec member x t =
    match t with
    | E -> false
    | T (_, a, y, b) ->
        if Element.lt x y then member x a
        else if Element.lt y x then member x b
        else true

  let balance = function
    | B, T (R, T (R, a, x, b), y, c), z, d (* LL *)
    | B, T (R, a, x, T (R, b, y, c)), z, d (* LR *)
    | B, a, x, T (R, T (R, b, y, c), z, d) (* RL *)
    | B, a, x, T (R, b, y, T (R, c, z, d)) (* RR *) ->
        T (R, T (B, a, x, b), y, T (B, c, z, d))
    | c, a, x, b -> T (c, a, x, b)

  let insert x s =
    let rec ins = function
      | E -> T (R, E, x, E)
      | T (color, a, y, b) as s ->
          if Element.lt x y then balance (color, ins a, y, b)
          else if Element.lt y x then balance (color, a, y, ins b)
          else s
    in
    match ins s with T (_, a, y, b) -> T (B, a, y, b) | E -> assert false
end
