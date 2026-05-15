open Sigs

module HoodMelvilleQueue : QUEUE = struct
  type 'a rotation_state =
    | Idle
    | Reversing of int * 'a list * 'a list * 'a list * 'a list
    | Appending of int * 'a list * 'a list
    | Done of 'a list

  exception EMPTY

  type 'a queue = int * 'a list * 'a rotation_state * int * 'a list

  let exec = function
    | Reversing (ok, x :: f, f', y :: r, r') ->
        Reversing (ok + 1, f, x :: f', r, y :: r')
    | Reversing (ok, [], f', [ y ], r') -> Appending (ok, f', y :: r')
    | Appending (0, _, r') -> Done r'
    | Appending (ok, x :: f', r') -> Appending (ok - 1, f', x :: r')
    | state -> state

  let invalidate = function
    | Reversing (ok, f, f', r, r') -> Reversing (ok - 1, f, f', r, r')
    | Appending (0, _, r') -> Done r'
    | Appending (ok, f', r') -> Appending (ok - 1, f', r')
    | state -> state

  let exec2 (lenf, f, state, lenr, r) =
    match exec (exec state) with
    | Done newf -> (lenf, newf, Idle, lenr, r)
    | newstate -> (lenf, f, newstate, lenr, r)

  let check ((lenf, f, _, lenr, r) as q) =
    if lenr <= lenf then exec2 q
    else
      let newstate = Reversing (0, f, [], r, []) in
      exec2 (lenf + lenr, f, newstate, 0, [])

  let empty = (0, [], Idle, 0, [])
  let is_empty (lenf, _, _, _, _) = lenf = 0
  let snoc (lenf, f, state, lenr, r) x = check (lenf, f, state, lenr + 1, x :: r)
  let head = function _, [], _, _, _ -> raise EMPTY | _, x :: _, _, _, _ -> x

  let tail = function
    | _, [], _, _, _ -> raise EMPTY
    | lenf, _ :: f, state, lenr, r ->
        check (lenf - 1, f, invalidate state, lenr, r)
end

module BankersDeque (C : CONST_INT) (S : STREAM) : DEQUE = struct
  open S

  let c = C.c

  type 'a queue = int * 'a stream * int * 'a stream

  exception EMPTY

  let empty = (0, lazy Nil, 0, lazy Nil)
  let is_empty (lenf, _, lenr, _) = lenf + lenr = 0

  let check ((lenf, f, lenr, r) as q) =
    if lenf > (c * lenr) + 1 then
      let i = (lenf + lenr) / 2 in
      let j = lenf + lenr - i in
      let f' = take i f in
      let r' = r ++ reverse (drop i f) in
      (i, f', j, r')
    else if lenr > (c * lenf) + 1 then
      let j = (lenf + lenr) / 2 in
      let i = lenf + lenr - j in
      let r' = take j r in
      let f' = f ++ reverse (drop j r) in
      (i, f', j, r')
    else q

  let cons x (lenf, f, lenr, r) = check (lenf + 1, lazy (Cons (x, f)), lenr, r)

  let head = function
    | _, (lazy Nil), _, (lazy Nil) -> raise EMPTY
    | _, (lazy Nil), _, (lazy (Cons (x, _))) -> x
    | _, (lazy (Cons (x, _))), _, _ -> x

  let tail = function
    | _, (lazy Nil), _, (lazy Nil) -> raise EMPTY
    | _, (lazy Nil), _, (lazy (Cons (_, _))) -> empty
    | lenf, (lazy (Cons (_, f'))), lenr, r -> check (lenf - 1, f', lenr, r)

  let snoc (lenf, f, lenr, r) x = check (lenf, f, lenr + 1, lazy (Cons (x, r)))

  let last = function
    | _, (lazy Nil), _, (lazy Nil) -> raise EMPTY
    | _, (lazy (Cons (x, _))), _, (lazy Nil) -> x
    | _, _, _, (lazy (Cons (x, _))) -> x

  let init = function
    | _, (lazy Nil), _, (lazy Nil) -> raise EMPTY
    | _, (lazy (Cons (_, _))), _, (lazy Nil) -> empty
    | lenf, f, lenr, (lazy (Cons (_, r'))) -> check (lenf, f, lenr - 1, r')
end
