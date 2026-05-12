open Sigs

module RealTimeQueue (S : STREAM) : QUEUE = struct
  open S

  type 'a queue = 'a stream * 'a list * 'a stream

  exception EMPTY

  let empty = (lazy Nil, [], lazy Nil)
  let is_empty = function (lazy Nil), _, _ -> true | _ -> false

  let rec rotate = function
    | (lazy Nil), y :: _, a -> lazy (Cons (y, a))
    | (lazy (Cons (x, xs))), y :: ys, a ->
        lazy (Cons (x, rotate (xs, ys, lazy (Cons (y, a)))))
    | _, [], _ -> assert false (* impossible by invariant |r| = |f| + 1 *)

  let exec = function
    | f, r, (lazy (Cons (_, s))) -> (f, r, s)
    | f, r, (lazy Nil) ->
        let f' = rotate (f, r, lazy Nil) in
        (f', [], f')

  let snoc (f, r, s) x = exec (f, x :: r, s)

  let head = function
    | (lazy Nil), _, _ -> raise EMPTY
    | (lazy (Cons (x, _))), _, _ -> x

  let tail = function
    | (lazy Nil), _, _ -> raise EMPTY
    | (lazy (Cons (_, f))), r, s -> exec (f, r, s)
end

module ScheduledBinomialHeap (Element : ORDERED) (S : STREAM) :
  HEAP with type Elem.t = Element.t = struct
  open S
  module Elem = Element

  type tree = Node of Elem.t * tree list
  type digit = Zero | One of tree
  type schedule = digit stream list
  type heap = digit stream * schedule

  exception EMPTY

  let empty = (lazy Nil, [])
  let is_empty = function (lazy Nil), _ -> true | _ -> false

  let link s t =
    let (Node (x1, c1)) = s in
    let (Node (x2, c2)) = t in
    if Elem.leq x1 x2 then Node (x1, t :: c1) else Node (x2, s :: c2)

  let rec ins_tree s t =
    match t with
    | (lazy Nil) -> lazy (Cons (One s, lazy Nil))
    | (lazy (Cons (Zero, ds))) -> lazy (Cons (One s, ds))
    | (lazy (Cons (One t', ds))) -> lazy (Cons (Zero, ins_tree (link s t') ds))

  let rec mrg s t =
    match (s, t) with
    | _, (lazy Nil) -> s
    | (lazy Nil), _ -> t
    | (lazy (Cons (Zero, s'))), (lazy (Cons (d, t'))) ->
        lazy (Cons (d, mrg s' t'))
    | (lazy (Cons (d, s'))), (lazy (Cons (Zero, t'))) ->
        lazy (Cons (d, mrg s' t'))
    | (lazy (Cons (One x, s'))), (lazy (Cons (One y, t'))) ->
        lazy (Cons (Zero, ins_tree (link x y) (mrg s' t')))

  let rec normalize = function
    | (lazy Nil) as ds -> ds
    | (lazy (Cons (_, ds'))) as ds ->
        ignore (normalize ds');
        ds

  let exec = function
    | [] -> []
    | (lazy (Cons (Zero, job))) :: sched -> job :: sched
    | _ :: sched -> sched

  let insert x (ds, sched) =
    let ds' = ins_tree (Node (x, [])) ds in
    (ds', exec (exec (ds' :: sched)))

  let merge (ds1, _) (ds2, _) =
    let ds = normalize (mrg ds1 ds2) in
    (ds, [])

  let rec remove_min_tree = function
    | (lazy Nil) -> raise EMPTY
    | (lazy (Cons (One t, (lazy Nil)))) -> (t, lazy Nil)
    | (lazy (Cons (Zero, ds))) ->
        let t', ds' = remove_min_tree ds in
        (t', lazy (Cons (Zero, ds')))
    | (lazy (Cons (One (Node (x, _) as t), ds))) ->
        let (Node (x', _) as t'), ds' = remove_min_tree ds in
        if Elem.leq x x' then (t, lazy (Cons (Zero, ds)))
        else (t', lazy (Cons (One t, ds')))

  let find_min (ds, _) =
    let Node (x, _), _ = remove_min_tree ds in
    x

  let delete_min (ds, _) =
    let Node (_, c), ds' = remove_min_tree ds in
    let ds'' = mrg (of_list List.(map (fun t -> One t) (rev c))) ds' in
    (normalize ds'', [])
end
