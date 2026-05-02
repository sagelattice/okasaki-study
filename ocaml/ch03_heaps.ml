open Ch02_binary_search_trees

module type HEAP = sig
  module Elem : ORDERED

  type heap

  val empty : heap
  val isEmpty : heap -> bool
  val insert : Elem.t -> heap -> heap
  val merge : heap -> heap -> heap
  val findMin : heap -> Elem.t
  val deleteMin : heap -> heap
end

module LeftistHeap (Element : ORDERED) : HEAP with type Elem.t = Element.t =
struct
  module Elem = Element

  exception EMPTY

  type heap = E | T of int * Elem.t * heap * heap

  let empty = E
  let isEmpty = function E -> true | _ -> false
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
  let findMin = function E -> raise EMPTY | T (_, x, _, _) -> x
  let deleteMin = function E -> raise EMPTY | T (_, _, a, b) -> merge a b
end

module BinomialHeap (Element : ORDERED) : HEAP with type Elem.t = Element.t =
struct
  module Elem = Element

  exception EMPTY

  type tree = Node of int * Elem.t * tree list
  type heap = tree list

  let empty = []
  let isEmpty = List.is_empty
  let rank (Node (r, _, _)) = r
  let root (Node (_, x, _)) = x

  let link (Node (r, x1, c1) as t1) (Node (_, x2, c2) as t2) =
    if Elem.leq x1 x2 then Node (r + 1, x1, t2 :: c1)
    else Node (r + 1, x2, t1 :: c2)

  let rec insTree t ts =
    match (t, ts) with
    | t, [] -> [ t ]
    | t, t' :: ts' ->
        if rank t < rank t' then t :: ts else insTree (link t t') ts'

  let insert x ts = insTree (Node (0, x, [])) ts

  let rec merge h1 h2 =
    match (h1, h2) with
    | ts1, [] -> ts1
    | [], ts2 -> ts2
    | (t1 :: ts1' as ts1), (t2 :: ts2' as ts2) ->
        if rank t1 < rank t2 then t1 :: merge ts1 ts2
        else if rank t2 < rank t1 then t2 :: merge ts1 ts2'
        else insTree (link t1 t2) (merge ts1' ts2')

  let rec removeMinTree = function
    | [] -> raise EMPTY
    | [ t ] -> (t, [])
    | t :: ts ->
        let t', ts' = removeMinTree ts in
        if Elem.leq (root t) (root t') then (t, ts) else (t', t :: ts')

  let findMin ts =
    let t, _ = removeMinTree ts in
    root t

  let deleteMin ts =
    let Node (_, _, ts1), ts2 = removeMinTree ts in
    merge (List.rev ts1) ts2
end

module TestHeap (H : HEAP with type Elem.t = int) = struct
  let run () =
    let open H in
    assert (isEmpty empty);
    assert (not (isEmpty (insert 1 empty)));
    assert (findMin (insert 1 empty) = 1);
    assert (findMin (insert 3 (insert 1 (insert 2 empty))) = 1);
    assert (findMin (deleteMin (insert 3 (insert 1 (insert 2 empty)))) = 2);
    let h1 = insert 3 (insert 1 empty) in
    let h2 = insert 4 (insert 2 empty) in
    assert (findMin (merge h1 h2) = 1);
    assert (findMin (deleteMin (merge h1 h2)) = 2);
    (* immutability *)
    let h = insert 1 empty in
    let _ = insert 0 h in
    assert (findMin h = 1);
    let h = insert 2 (insert 1 empty) in
    let _ = deleteMin h in
    assert (findMin h = 1);
    print_endline "passed"
end

let () =
  let module T1 = TestHeap (LeftistHeap (IntOrder)) in
  let module T2 = TestHeap (BinomialHeap (IntOrder)) in
  print_endline "heaps";
  print_string "leftist heap: ";
  T1.run ();
  print_string "binomial heap: ";
  T2.run ()
