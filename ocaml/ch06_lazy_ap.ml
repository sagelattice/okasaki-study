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
