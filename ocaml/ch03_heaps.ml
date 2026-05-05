open Ch02_binary_search_trees

module type HEAP = sig
  module Elem : ORDERED

  exception EMPTY

  type heap

  val empty : heap
  val is_empty : heap -> bool
  val insert : Elem.t -> heap -> heap
  val merge : heap -> heap -> heap
  val find_min : heap -> Elem.t
  val delete_min : heap -> heap
end

module LeftistHeap (Element : ORDERED) : HEAP with type Elem.t = Element.t =
struct
  module Elem = Element

  type heap = E | T of int * Elem.t * heap * heap

  exception EMPTY

  let empty = E
  let is_empty = function E -> true | _ -> false
  let rank = function E -> 0 | T (r, _, _, _) -> r

  let makeT x a b =
    if rank a >= rank b then T (rank b + 1, x, a, b) else T (rank a + 1, x, b, a)

  let rec merge h1 h2 =
    match (h1, h2) with
    | h, E -> h
    | E, h -> h
    | T (_, x, a1, b1), T (_, y, a2, b2) ->
        if Elem.leq x y then makeT x a1 (merge b1 h2)
        else makeT y a2 (merge h1 b2)

  let insert x h = merge (T (1, x, E, E)) h
  let find_min = function E -> raise EMPTY | T (_, x, _, _) -> x
  let delete_min = function E -> raise EMPTY | T (_, _, a, b) -> merge a b
end

module BinomialHeap (Element : ORDERED) : HEAP with type Elem.t = Element.t =
struct
  module Elem = Element

  exception EMPTY

  type tree = Node of int * Elem.t * tree list
  type heap = tree list

  let empty = []
  let is_empty = List.is_empty
  let rank (Node (r, _, _)) = r
  let root (Node (_, x, _)) = x

  let link (Node (r, x1, c1) as t1) (Node (_, x2, c2) as t2) =
    if Elem.leq x1 x2 then Node (r + 1, x1, t2 :: c1)
    else Node (r + 1, x2, t1 :: c2)

  let rec ins_tree t ts =
    match (t, ts) with
    | t, [] -> [ t ]
    | t, t' :: ts' ->
        if rank t < rank t' then t :: ts else ins_tree (link t t') ts'

  let insert x ts = ins_tree (Node (0, x, [])) ts

  let rec merge h1 h2 =
    match (h1, h2) with
    | ts1, [] -> ts1
    | [], ts2 -> ts2
    | (t1 :: ts1' as ts1), (t2 :: ts2' as ts2) ->
        if rank t1 < rank t2 then t1 :: merge ts1' ts2
        else if rank t2 < rank t1 then t2 :: merge ts1 ts2'
        else ins_tree (link t1 t2) (merge ts1' ts2')

  let rec remove_min_tree = function
    | [] -> raise EMPTY
    | [ t ] -> (t, [])
    | t :: ts ->
        let t', ts' = remove_min_tree ts in
        if Elem.leq (root t) (root t') then (t, ts) else (t', t :: ts')

  let find_min ts =
    let t, _ = remove_min_tree ts in
    root t

  let delete_min ts =
    let Node (_, _, ts1), ts2 = remove_min_tree ts in
    merge (List.rev ts1) ts2
end
