open Sigs

module Stream : STREAM = struct
  type 'a stream_cell = Nil | Cons of 'a * 'a stream
  and 'a stream = 'a stream_cell Lazy.t

  let rec ( ++ ) s t =
    lazy
      (match s with
      | (lazy Nil) -> Lazy.force t
      | (lazy (Cons (x, s'))) -> Cons (x, s' ++ t))

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
    let rec reverse' s' acc =
      match s' with
      | (lazy Nil) -> acc
      | (lazy (Cons (x, rest))) -> reverse' rest (Cons (x, lazy acc))
    in
    lazy (reverse' s Nil)

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
