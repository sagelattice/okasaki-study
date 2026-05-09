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
