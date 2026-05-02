open Ch02_binary_search_trees

module RedBlackSet (Element : ORDERED) = struct
  type elem = Element.t
  type color = R | B
  type tree = E | T of color * tree * elem * tree
  type set = tree

  let empty = E

  let rec member x t =
    match t with
    | E -> false
    | T (_, a, y, b) ->
        if Element.lt x y then member x a
        else if Element.lt y x then member x b
        else true

  let balance = function
    | B, T (R, T (R, a, x, b), y, c), z, d
    | B, T (R, a, x, T (R, b, y, c)), z, d
    | B, a, x, T (R, T (R, b, y, c), z, d)
    | B, a, x, T (R, b, y, T (R, c, z, d)) ->
        T (R, T (B, a, x, b), y, T (B, c, z, d))
    | c, a, x, b -> T (c, a, x, b)

  let insert x s =
    let rec ins = function
      | E -> T (R, E, x, E)
      | T (color, a, y, b) as s ->
          if Element.lt x y then balance (color, ins a, y, b)
          else if Element.lt y x then balance (color, a, y, ins b)
          else s
    in
    match ins s with T (_, a, y, b) -> T (B, a, y, b) | E -> assert false
end

let () =
  let module T = TestSet (RedBlackSet (IntOrder)) in
  let module RB = RedBlackSet (IntOrder) in
  print_endline "red black tree";
  print_string "red black set: ";
  let rec red_ok = function
    | RB.E -> true
    | RB.T (RB.R, RB.T (RB.R, _, _, _), _, _)
    | RB.T (RB.R, _, _, RB.T (RB.R, _, _, _)) ->
        false
    | RB.T (_, l, _, r) -> red_ok l && red_ok r
  in
  let rec black_height = function
    | RB.E -> Some 0
    | RB.T (c, l, _, r) -> (
        match (black_height l, black_height r) with
        | Some lh, Some rh when lh = rh -> Some (lh + if c = RB.B then 1 else 0)
        | _ -> None)
  in
  let cases =
    [
      (* RR *)
      [ 1; 2; 3 ];
      (* LL *)
      [ 3; 2; 1 ];
      (* RL *)
      [ 1; 3; 2 ];
      (* LR *)
      [ 3; 1; 2 ];
    ]
  in
  List.iter
    (fun xs ->
      let s = List.fold_left (fun acc x -> RB.insert x acc) RB.empty xs in
      assert (red_ok s);
      (* perfectly balanced, as all things should be *)
      assert (black_height s <> None))
    cases;
  T.run ()
