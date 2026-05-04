open Okasaki

(* Generic SET tests reused here for the red-black implementation *)
module SetTests (S : Ch02_binary_search_trees.SET with type elem = int) = struct
  open S

  (* the empty set contains no elements *)
  let test_empty_has_no_members () =
    Alcotest.(check bool) "1 not in empty" false (member 1 empty)

  (* the inserted element is found in the resulting set *)
  let test_singleton_member () =
    Alcotest.(check bool) "1 in {1}" true (member 1 (insert 1 empty))

  (* the original element is still present after a second insertion *)
  let test_member_after_second_insert () =
    let s = insert 1 empty in
    Alcotest.(check bool) "1 still in {1,2}" true (member 1 (insert 2 s))

  (* the newly inserted element is found *)
  let test_new_element_member () =
    let s = insert 1 empty in
    Alcotest.(check bool) "2 in {1,2}" true (member 2 (insert 2 s))

  (* inserting into s does not affect s itself *)
  let test_immutability () =
    let s = insert 1 empty in
    ignore (insert 2 s);
    Alcotest.(check bool) "2 not in {1} after insert 2" false (member 2 s)

  (* inserting a duplicate returns the same tree structure (sharing) *)
  let test_duplicate_insert () =
    let s = insert 1 empty in
    Alcotest.(check bool) "insert 1 {1} = {1}" true (insert 1 s = s)

  let tests =
    Alcotest.
      [
        test_case "empty has no members"         `Quick test_empty_has_no_members;
        test_case "singleton member"             `Quick test_singleton_member;
        test_case "member after second insert"   `Quick test_member_after_second_insert;
        test_case "newly inserted element found" `Quick test_new_element_member;
        test_case "immutability"                 `Quick test_immutability;
        test_case "duplicate insert is no-op"    `Quick test_duplicate_insert;
      ]
end

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
