open Okasaki

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

module UST =
  SetTests
    (Ch02_binary_search_trees.UnbalancedSet (Ch02_binary_search_trees.IntOrder))

let () =
  Alcotest.run "ch02 binary search trees"
    [
      ("UnbalancedSet", UST.tests);
    ]
