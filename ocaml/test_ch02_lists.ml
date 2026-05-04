open Okasaki

module StackTests (S : Ch02_lists.STACK) = struct
  module Ops = Ch02_lists.StackOps (S)

  open S
  open Ops

  (* appending empty on the left leaves the stack unchanged *)
  let test_concat_left_id () =
    let s = cons 1 (cons 2 (cons 3 empty)) in
    Alcotest.(check bool) "empty ++ s = s" true (empty ++ s = s)

  (* appending empty on the right leaves the stack unchanged *)
  let test_concat_right_id () =
    let s = cons 1 (cons 2 (cons 3 empty)) in
    Alcotest.(check bool) "s ++ empty = s" true (s ++ empty = s)

  (* concatenation preserves element order *)
  let test_concat_order () =
    let s = cons 1 (cons 2 (cons 3 empty)) in
    let t = cons 4 empty in
    let expected = cons 1 (cons 2 (cons 3 (cons 4 empty))) in
    Alcotest.(check bool) "s ++ t order" true (s ++ t = expected)

  (* update at index 0 replaces the head *)
  let test_update_head () =
    let s = cons 1 (cons 2 (cons 3 empty)) in
    Alcotest.(check bool) "update head"
      true (update s 0 0 = cons 0 (cons 2 (cons 3 empty)))

  (* update at index 1 replaces the second element *)
  let test_update_middle () =
    let s = cons 1 (cons 2 (cons 3 empty)) in
    Alcotest.(check bool) "update middle"
      true (update s 1 0 = cons 1 (cons 0 (cons 3 empty)))

  (* update with an out-of-bounds index raises SUBSCRIPT *)
  let test_update_subscript () =
    let s = cons 1 (cons 2 (cons 3 empty)) in
    let raised = try ignore (update s 4 0); false with SUBSCRIPT -> true in
    Alcotest.(check bool) "out-of-bounds raises SUBSCRIPT" true raised

  (* update does not mutate the original stack *)
  let test_immutability () =
    let s = cons 1 (cons 2 (cons 3 empty)) in
    ignore (update s 0 99);
    Alcotest.(check bool) "s unchanged after update"
      true (s = cons 1 (cons 2 (cons 3 empty)))

  let tests =
    Alcotest.
      [
        test_case "concat left identity"      `Quick test_concat_left_id;
        test_case "concat right identity"     `Quick test_concat_right_id;
        test_case "concat preserves order"    `Quick test_concat_order;
        test_case "update head"               `Quick test_update_head;
        test_case "update middle element"     `Quick test_update_middle;
        test_case "update out-of-bounds"      `Quick test_update_subscript;
        test_case "immutability after update" `Quick test_immutability;
      ]
end

module LST = StackTests (Ch02_lists.ListStack)
module CST = StackTests (Ch02_lists.CustomStack)

let () =
  Alcotest.run "ch02 lists"
    [
      ("ListStack",   LST.tests);
      ("CustomStack", CST.tests);
    ]
