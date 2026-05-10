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

  let find_min (lazy ts) =
    let t, _ = remove_min_tree ts in
    root t

  let delete_min ts =
    let Node (_, _, ts1), ts2 = remove_min_tree (Lazy.force ts) in
    lazy (mrg (List.rev ts1) ts2)
end

module PhysicistsQueue : QUEUE = struct
  type 'a queue = 'a list * int * 'a list Lazy.t * int * 'a list

  exception EMPTY

  let empty = ([], 0, lazy [], 0, [])
  let is_empty (_, lenf, _, _, _) = lenf = 0

  let checkw = function
    | [], lenf, f, lenr, r -> (Lazy.force f, lenf, f, lenr, r)
    | q -> q

  let check q =
    match q with
    | _, lenf, f, lenr, r ->
        if lenr <= lenf then checkw q
        else
          let (lazy f') = f in
          checkw (f', lenf + lenr, lazy (f' @ List.rev r), 0, [])

  let snoc q x =
    let w, lenf, f, lenr, r = q in
    check (w, lenf, f, lenr + 1, x :: r)

  let head = function [], _, _, _, _ -> raise EMPTY | x :: _, _, _, _, _ -> x

  let tail = function
    | [], _, _, _, _ -> raise EMPTY
    | _ :: w, lenf, f, lenr, r ->
        check (w, lenf - 1, Lazy.map List.tl f, lenr, r)
end

module BottomUpMergeSort (Element : ORDERED) :
  SORTABLE with type Elem.t = Element.t = struct
  module Elem = Element

  type sortable = int * Elem.t list list Lazy.t

  let rec mrg xs ys =
    match (xs, ys) with
    | [], _ -> ys
    | _, [] -> xs
    | x :: xs', y :: ys' ->
        if Elem.leq x y then x :: mrg xs' ys else y :: mrg xs ys'

  let empty = (0, lazy [])

  let add x (size, segs) =
    let rec add_seg (seg, segs, size) =
      if size mod 2 = 0 then seg :: segs
      else add_seg (mrg seg (List.hd segs), List.tl segs, size / 2)
    in
    (size + 1, lazy (add_seg ([ x ], Lazy.force segs, size)))

  let sort (_, (lazy segs)) =
    let rec mrg_all xs ys =
      match (xs, ys) with
      | xs, [] -> xs
      | xs, seg :: segs -> mrg_all (mrg xs seg) segs
    in
    mrg_all [] segs
end
