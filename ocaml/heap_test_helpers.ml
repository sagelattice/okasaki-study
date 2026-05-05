open Okasaki

module HeapTests (H : Ch03_heaps.HEAP with type Elem.t = int) = struct
  open H

  let test_empty_is_empty () =
    Alcotest.(check bool) "empty is empty" true (is_empty empty)

  let test_nonempty_after_insert () =
    Alcotest.(check bool) "insert 1 is not empty" false (is_empty (insert 1 empty))

  let test_findmin_singleton () =
    Alcotest.(check int) "find_min {1} = 1" 1 (find_min (insert 1 empty))

  let test_findmin_three () =
    Alcotest.(check int) "find_min {3,1,2} = 1" 1
      (find_min (insert 3 (insert 1 (insert 2 empty))))

  let test_deletemin_exposes_next () =
    Alcotest.(check int) "find_min after delete_min = 2" 2
      (find_min (delete_min (insert 3 (insert 1 (insert 2 empty)))))

  let test_merge_min () =
    let h1 = insert 3 (insert 1 empty) in
    let h2 = insert 4 (insert 2 empty) in
    Alcotest.(check int) "find_min (merge {1,3} {2,4}) = 1" 1 (find_min (merge h1 h2))

  let test_merge_second_min () =
    let h1 = insert 3 (insert 1 empty) in
    let h2 = insert 4 (insert 2 empty) in
    Alcotest.(check int) "second min after merge = 2" 2
      (find_min (delete_min (merge h1 h2)))

  let test_merge_new_min () =
    let h1 = insert 3 (insert 2 empty) in
    Alcotest.(check int) "find_min (merge {2,3} {0}) = 0" 0
      (find_min (merge h1 (insert 0 empty)))

  let test_findmin_raises_on_empty () =
    Alcotest.check_raises "find_min [] raises" EMPTY
      (fun () -> ignore (find_min empty))

  let test_deletemin_raises_on_empty () =
    Alcotest.check_raises "delete_min [] raises" EMPTY
      (fun () -> ignore (delete_min empty))

  let test_immutability_insert () =
    let h = insert 1 empty in
    ignore (insert 0 h);
    Alcotest.(check int) "h unchanged after insert 0 h" 1 (find_min h)

  let test_immutability_deletemin () =
    let h = insert 2 (insert 1 empty) in
    ignore (delete_min h);
    Alcotest.(check int) "h unchanged after delete_min h" 1 (find_min h)

  let tests =
    Alcotest.
      [
        test_case "empty is empty"                 `Quick test_empty_is_empty;
        test_case "non-empty after insert"         `Quick test_nonempty_after_insert;
        test_case "findMin singleton"              `Quick test_findmin_singleton;
        test_case "findMin three elements"         `Quick test_findmin_three;
        test_case "deleteMin exposes next minimum" `Quick test_deletemin_exposes_next;
        test_case "merge findMin"                  `Quick test_merge_min;
        test_case "merge second minimum"           `Quick test_merge_second_min;
        test_case "merge new minimum"              `Quick test_merge_new_min;
        test_case "findMin raises on empty"        `Quick test_findmin_raises_on_empty;
        test_case "deleteMin raises on empty"      `Quick test_deletemin_raises_on_empty;
        test_case "immutability: insert"           `Quick test_immutability_insert;
        test_case "immutability: deleteMin"        `Quick test_immutability_deletemin;
      ]
end
