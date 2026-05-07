open Ch04_streams
open Ch05_amortization

module BankersQueue : QUEUE = struct
  type 'a stream = 'a Stream.stream
  type 'a queue = int * 'a stream * int * 'a stream

  exception EMPTY

  let empty = (0, lazy Stream.Nil, 0, lazy Stream.Nil)
  let is_empty = function lenf, _, _, _ -> lenf = 0

  let check q =
    let lenf, f, lenr, r = q in
    if lenr <= lenf then q else Stream.(lenf + lenr, f ++ reverse r, 0, lazy Nil)

  let snoc q x =
    let lenf, f, lenr, r = q in
    check (lenf, f, lenr + 1, lazy (Stream.Cons (x, r)))

  let head = function
    | _, (lazy Stream.Nil), _, _ -> raise EMPTY
    | _, (lazy (Stream.Cons (x, _))), _, _ -> x

  let tail = function
    | _, (lazy Stream.Nil), _, _ -> raise EMPTY
    | lenf, (lazy (Stream.Cons (_, f'))), lenr, r ->
        check (lenf - 1, f', lenr, r)
end
