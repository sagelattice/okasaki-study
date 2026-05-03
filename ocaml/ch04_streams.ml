module type STREAM = sig
  type 'a stream_cell = Nil | Cons of 'a * 'a stream
  and 'a stream = 'a stream_cell Lazy.t

  val ( ++ ) : 'a stream -> 'a stream -> 'a stream
  val take : int -> 'a stream -> 'a stream
  val drop : int -> 'a stream -> 'a stream
  val reverse : 'a stream -> 'a stream

  (* the book doesn't define these and I added them for testing *)
  val of_list : 'a list -> 'a stream
  val to_list : 'a stream -> 'a list
end

module Stream : STREAM = struct
  type 'a stream_cell = Nil | Cons of 'a * 'a stream
  and 'a stream = 'a stream_cell Lazy.t

  let rec ( ++ ) s t =
    match s with
    | (lazy Nil) -> t
    | (lazy (Cons (x, s'))) -> lazy (Cons (x, s' ++ t))

  let rec take n s =
    match (n, s) with
    (* this can't be reduced with an or-pattern because n isn't in both *)
    | 0, _ -> lazy Nil
    | _, (lazy Nil) -> lazy Nil
    | n, (lazy (Cons (x, s'))) -> lazy (Cons (x, take (n - 1) s'))

  let rec drop n s =
    match (n, s) with
    | 0, _ -> s
    | _, (lazy Nil) -> lazy Nil
    | n, (lazy (Cons (_, s'))) -> drop (n - 1) s'

  let reverse s =
    let rec reverse' s' r =
      match (s', r) with
      | (lazy Nil), r -> r
      | (lazy (Cons (x, rest))), r -> reverse' rest (lazy (Cons (x, r)))
    in
    reverse' s (lazy Nil)

  let of_list xs =
    List.fold_right (fun x acc -> lazy (Cons (x, acc))) xs (lazy Nil)

  let to_list s =
    let rec to_list' s acc =
      match Lazy.force s with
      | Nil -> List.rev acc
      | Cons (x, s') -> to_list' s' (x :: acc)
    in
    to_list' s []
end

let () =
  let xs = [ 1; 2; 3 ] in
  let s = Stream.of_list xs in
  print_string "streams: ";
  assert (xs = Stream.to_list s);
  assert (List.drop 1 xs = Stream.to_list (Stream.drop 1 s));
  assert ([] = Stream.to_list (Stream.drop 42 s));
  assert (List.drop 0 xs = Stream.to_list (Stream.drop 0 s));
  assert (List.take 1 xs = Stream.to_list (Stream.take 1 s));
  assert (xs = Stream.to_list (Stream.take 42 s));
  assert (List.take 0 xs = Stream.to_list (Stream.take 0 s));
  assert (List.rev xs = Stream.to_list (Stream.reverse s));
  assert ([ 1; 2; 3; 4 ] = Stream.(to_list (s ++ of_list [ 4 ])));
  assert (xs = Stream.(to_list (s ++ lazy Nil)));
  assert (xs = Stream.(to_list (lazy Nil ++ s)));
  print_endline "passed"
