open Ch02_binary_search_trees
open Ch03_heaps

module type QUEUE = sig
  type 'a queue

  exception EMPTY

  val empty : 'a queue
  val is_empty : 'a queue -> bool
  val snoc : 'a queue -> 'a -> 'a queue
  val head : 'a queue -> 'a
  val tail : 'a queue -> 'a queue
end

module BatchedQueue : QUEUE = struct
  type 'a queue = 'a list * 'a list

  exception EMPTY

  let empty = ([], [])
  let is_empty = function [], _ -> true | _, _ -> false
  let checkf q = match q with [], r -> (List.rev r, []) | _ -> q

  let snoc q x =
    let f, r = q in
    checkf (f, x :: r)

  let head = function [], _ -> raise EMPTY | x :: _, _ -> x
  let tail = function [], _ -> raise EMPTY | _ :: f, r -> checkf (f, r)
end

module SplayHeap (Element : ORDERED) : HEAP with type Elem.t = Element.t = struct
  module Elem = Element

  type heap = E | T of heap * Elem.t * heap

  exception EMPTY

  let empty = E
  let is_empty = function E -> true | _ -> false

  let rec partition pivot t =
    match t with
    | E -> (E, E)
    | T (a, x, b) as t' -> (
        if Element.leq x pivot then
          match b with
          | E -> (t', E)
          | T (b1, y, b2) ->
              if Element.leq y pivot then
                let small, big = partition pivot b2 in
                (T (T (a, x, b1), y, small), big)
              else
                let small, big = partition pivot b1 in
                (T (a, x, small), T (big, y, b2))
        else
          match a with
          | E -> (E, t')
          | T (a1, y, a2) ->
              if Element.leq y pivot then
                let small, big = partition pivot a2 in
                (T (a1, y, small), T (big, x, b))
              else
                let small, big = partition pivot a1 in
                (small, T (big, y, T (a2, x, b))))

  let insert x t =
    let a, b = partition x t in
    T (a, x, b)

  let rec merge s t =
    match s with
    | E -> t
    | T (a, x, b) ->
        let ta, tb = partition x t in
        T (merge ta a, x, merge tb b)

  let rec find_min = function
    | E -> raise EMPTY
    | T (E, x, _) -> x
    | T (a, _, _) -> find_min a

  let rec delete_min = function
    | E -> raise EMPTY
    | T (E, _, b) -> b
    | T (T (E, _, b), y, c) -> T (b, y, c)
    | T (T (a, x, b), y, c) -> T (delete_min a, x, T (b, y, c))
end
