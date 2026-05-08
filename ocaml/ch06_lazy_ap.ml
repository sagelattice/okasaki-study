open Sigs

module BankersQueue (S : STREAM) : QUEUE = struct
  open S

  type 'a queue = int * 'a stream * int * 'a stream

  exception EMPTY

  let empty = (0, lazy Nil, 0, lazy Nil)
  let is_empty = function lenf, _, _, _ -> lenf = 0

  let check q =
    let lenf, f, lenr, r = q in
    if lenr <= lenf then q else (lenf + lenr, f ++ reverse r, 0, lazy Nil)

  let snoc q x =
    let lenf, f, lenr, r = q in
    check (lenf, f, lenr + 1, lazy (Cons (x, r)))

  let head = function
    | _, (lazy Nil), _, _ -> raise EMPTY
    | _, (lazy (Cons (x, _))), _, _ -> x

  let tail = function
    | _, (lazy Nil), _, _ -> raise EMPTY
    | lenf, (lazy (Cons (_, f'))), lenr, r -> check (lenf - 1, f', lenr, r)
end

module LazyBinomialHeap (Element : ORDERED) :
  HEAP with type Elem.t = Element.t = struct
  module Elem = Element

  type tree = Node of int * Elem.t * tree list
  type heap = tree list Lazy.t

  exception EMPTY

  let empty = lazy []
  let is_empty (lazy ts) = List.is_empty ts
  let rank (Node (r, _, _)) = r
  let root (Node (_, x, _)) = x

  let link s t =
    match (s, t) with
    | Node (r, x1, c1), Node (_, x2, c2) ->
        if Elem.leq x1 x2 then Node (r + 1, x1, t :: c1)
        else Node (r + 1, x2, s :: c2)

  let rec ins_tree s t =
    match t with
    | [] -> [ s ]
    | t' :: ts ->
        if rank s < rank t' then s :: t' :: ts else ins_tree (link s t') ts

  let rec mrg s t =
    match (s, t) with
    | _, [] -> s
    | [], _ -> t
    | s' :: ss', t' :: ts' ->
        if rank s' < rank t' then s' :: mrg ss' t
        else if rank t' < rank s' then t' :: mrg s ts'
        else ins_tree (link s' t') (mrg ss' ts')

  let insert x ts = lazy (ins_tree (Node (0, x, [])) (Lazy.force ts))
  let merge s t = lazy (mrg (Lazy.force s) (Lazy.force t))

  let rec remove_min_tree = function
    | [] -> raise EMPTY
    | [ t ] -> (t, [])
    | t :: ts ->
        let t', ts' = remove_min_tree ts in
        if Elem.leq (root t) (root t') then (t, ts) else (t', t :: ts')

  let find_min ts =
    let t, _ = remove_min_tree (Lazy.force ts) in
    root t

  let delete_min ts =
    let Node (_, _, ts1), ts2 = remove_min_tree (Lazy.force ts) in
    lazy (mrg (List.rev ts1) ts2)
end
