open Okasaki
open Okasaki_test_helpers.Set_test_helpers

module RBST =
  SetTests
    (Ch03_red_black_trees.RedBlackSet (Ch02_binary_search_trees.IntOrder))

(* Red-black invariant tests *)

module RB =
  Ch03_red_black_trees.RedBlackSet (Ch02_binary_search_trees.IntOrder)

(* no red node has a red child *)
let rec red_ok = function
  | RB.E -> true
  | RB.T (RB.R, RB.T (RB.R, _, _, _), _, _)
  | RB.T (RB.R, _, _, RB.T (RB.R, _, _, _)) ->
      false
  | RB.T (_, l, _, r) -> red_ok l && red_ok r

(* every root-to-leaf path has the same number of black nodes *)
let rec black_height = function
  | RB.E -> Some 0
  | RB.T (c, l, _, r) -> (
      match (black_height l, black_height r) with
      | Some lh, Some rh when lh = rh ->
          Some (lh + if c = RB.B then 1 else 0)
      | _ -> None)

let make_inv_test label xs () =
  let s = List.fold_left (fun acc x -> RB.insert x acc) RB.empty xs in
  Alcotest.(check bool) (label ^ ": red invariant") true (red_ok s);
  Alcotest.(check bool) (label ^ ": uniform black height") true
    (black_height s <> None)

let inv_tests =
  Alcotest.
    [
      (* ascending insertion triggers right-right rotation *)
      test_case "RR rotation preserves invariants" `Quick
        (make_inv_test "RR" [ 1; 2; 3 ]);
      (* descending insertion triggers left-left rotation *)
      test_case "LL rotation preserves invariants" `Quick
        (make_inv_test "LL" [ 3; 2; 1 ]);
      (* triggers right-left double rotation *)
      test_case "RL rotation preserves invariants" `Quick
        (make_inv_test "RL" [ 1; 3; 2 ]);
      (* triggers left-right double rotation *)
      test_case "LR rotation preserves invariants" `Quick
        (make_inv_test "LR" [ 3; 1; 2 ]);
    ]

let () =
  Alcotest.run "ch03 red black trees"
    [
      ("RedBlackSet",  RBST.tests);
      ("invariants",   inv_tests);
    ]
