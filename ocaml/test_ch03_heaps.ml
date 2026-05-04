open Okasaki

module HeapTests (H : Ch03_heaps.HEAP with type Elem.t = int) = struct
  open H

  (* a freshly created heap reports itself as empty *)
  let test_empty_is_empty () =
    Alcotest.(check bool) "empty is empty" true (isEmpty empty)

  (* inserting one element makes the heap non-empty *)
  let test_nonempty_after_insert () =
    Alcotest.(check bool) "insert 1 is not empty" false (isEmpty (insert 1 empty))

  (* the only element in a singleton heap is the minimum *)
  let test_findmin_singleton () =
    Alcotest.(check int) "findMin {1} = 1" 1 (findMin (insert 1 empty))

  (* findMin returns the global minimum regardless of insertion order *)
  let test_findmin_three () =
    Alcotest.(check int) "findMin {3,1,2} = 1" 1
      (findMin (insert 3 (insert 1 (insert 2 empty))))

  (* deleteMin exposes the second-smallest element *)
  let test_deletemin_exposes_next () =
    Alcotest.(check int) "findMin after deleteMin = 2" 2
      (findMin (deleteMin (insert 3 (insert 1 (insert 2 empty)))))

  (* merging two same-sized heaps preserves the global minimum *)
  let test_merge_equal_rank_min () =
    let h1 = insert 3 (insert 1 empty) in
    let h2 = insert 4 (insert 2 empty) in
    Alcotest.(check int) "findMin (merge {1,3} {2,4}) = 1" 1 (findMin (merge h1 h2))

  (* after deleteMin on the merged heap the second-smallest is correct *)
  let test_merge_equal_rank_second () =
    let h1 = insert 3 (insert 1 empty) in
    let h2 = insert 4 (insert 2 empty) in
    Alcotest.(check int) "second min after merge = 2" 2
      (findMin (deleteMin (merge h1 h2)))

  (* merging a singleton on the left with a larger heap *)
  let test_merge_unequal_rank_left () =
    let h2 = insert 4 (insert 2 empty) in
    Alcotest.(check int) "findMin (merge {1} {2,4}) = 1" 1
      (findMin (merge (insert 1 empty) h2))

  (* merging a heap with a singleton that holds the new minimum *)
  let test_merge_unequal_rank_right () =
    let h1 = insert 3 (insert 1 empty) in
    Alcotest.(check int) "findMin (merge {1,3} {0}) = 0" 0
      (findMin (merge h1 (insert 0 empty)))

  (* inserting into a heap does not mutate the original *)
  let test_immutability_insert () =
    let h = insert 1 empty in
    ignore (insert 0 h);
    Alcotest.(check int) "h unchanged after insert 0 h" 1 (findMin h)

  (* deleteMin does not mutate the original heap *)
  let test_immutability_deletemin () =
    let h = insert 2 (insert 1 empty) in
    ignore (deleteMin h);
    Alcotest.(check int) "h unchanged after deleteMin h" 1 (findMin h)

  let tests =
    Alcotest.
      [
        test_case "empty is empty"                    `Quick test_empty_is_empty;
        test_case "non-empty after insert"            `Quick test_nonempty_after_insert;
        test_case "findMin singleton"                 `Quick test_findmin_singleton;
        test_case "findMin three elements"            `Quick test_findmin_three;
        test_case "deleteMin exposes next minimum"    `Quick test_deletemin_exposes_next;
        test_case "merge equal rank findMin"          `Quick test_merge_equal_rank_min;
        test_case "merge equal rank second min"       `Quick test_merge_equal_rank_second;
        test_case "merge unequal rank left singleton" `Quick test_merge_unequal_rank_left;
        test_case "merge unequal rank right minimum"  `Quick test_merge_unequal_rank_right;
        test_case "immutability: insert"              `Quick test_immutability_insert;
        test_case "immutability: deleteMin"           `Quick test_immutability_deletemin;
      ]
end

module LHT = HeapTests (Ch03_heaps.LeftistHeap (Ch02_binary_search_trees.IntOrder))
module BHT = HeapTests (Ch03_heaps.BinomialHeap (Ch02_binary_search_trees.IntOrder))

let () =
  Alcotest.run "ch03 heaps"
    [
      ("LeftistHeap",  LHT.tests);
      ("BinomialHeap", BHT.tests);
    ]
