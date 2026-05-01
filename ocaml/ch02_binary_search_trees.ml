module type SET = sig
  type elem
  type set

  val empty : set
  val insert : elem -> set -> set
  val member : elem -> set -> bool
end

module type ORDERED = sig
  type t

  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool
end

module UnbalancedSet (Element : ORDERED) : SET with type elem = Element.t = struct
  type elem = Element.t
  type tree = E | T of tree * elem * tree
  type set = tree

  let empty = E

  let rec member x t = match t with
    | E -> false
    | T (left, y, right) ->
       if Element.lt x y then member x left
       else if Element.lt y x then member x right
       else true

  let rec insert x s = match s with
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

module TestSet (S : SET with type elem = int) = struct
  let run () =
    let open S in
    let s = insert 1 empty in
    assert (not (member 1 empty));
    assert (member 1 s);
    assert (member 1 (insert 2 s));
    assert (member 2 (insert 2 s));
    (* not mutable *)
    assert (not (member 2 s));
    (* instance equality *)
    assert (insert 1 s == s);
    print_endline "passed"
end

let () =
  let module T = TestSet(UnbalancedSet(IntOrder)) in
  T.run ()

let () = print_endline "all tests passed"

