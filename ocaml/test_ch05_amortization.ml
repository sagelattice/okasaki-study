open Okasaki
open Okasaki_test_helpers.Heap_test_helpers

let to_list q =
  let open Ch05_amortization.BatchedQueue in
  let rec go acc q =
    if is_empty q then List.rev acc
    else go (head q :: acc) (tail q)
  in
  go [] q

let build xs =
  let open Ch05_amortization.BatchedQueue in
  List.fold_left snoc empty xs

let queue_tests =
  let open Ch05_amortization.BatchedQueue in
  Alcotest.
    [
      test_case "empty is_empty" `Quick (fun () ->
        Alcotest.(check bool) "empty queue is empty" true (is_empty empty));

      test_case "snoc makes non-empty" `Quick (fun () ->
        Alcotest.(check bool) "singleton is not empty" false
          (is_empty (snoc empty 1)));

      test_case "snoc / head round-trip" `Quick (fun () ->
        Alcotest.(check int) "head of singleton" 1 (head (snoc empty 1)));

      test_case "FIFO order preserved" `Quick (fun () ->
        Alcotest.(check (list int)) "elements come out in insertion order"
          [ 1; 2; 3 ] (to_list (build [ 1; 2; 3 ])));

      test_case "tail removes head" `Quick (fun () ->
        Alcotest.(check int) "head after tail" 2
          (head (tail (build [ 1; 2; 3 ]))));

      test_case "tail to empty" `Quick (fun () ->
        Alcotest.(check bool) "tail of singleton is empty" true
          (is_empty (tail (snoc empty 42))));

      test_case "head raises on empty" `Quick (fun () ->
        Alcotest.check_raises "head [] raises" Ch05_amortization.BatchedQueue.EMPTY
          (fun () -> ignore (head empty)));

      test_case "tail raises on empty" `Quick (fun () ->
        Alcotest.check_raises "tail [] raises" Ch05_amortization.BatchedQueue.EMPTY
          (fun () -> ignore (tail empty)));

      test_case "checkf triggers on tail drain" `Quick (fun () ->
        let q = build [ 1; 2; 3 ] in
        let q = tail (tail q) in
        Alcotest.(check (list int)) "remaining element after rotation"
          [ 3 ] (to_list q));

      test_case "large queue round-trip" `Quick (fun () ->
        let xs = List.init 100 (fun i -> i) in
        Alcotest.(check (list int)) "100-element round-trip" xs
          (to_list (build xs)));
    ]

module SHT = HeapTests (Ch05_amortization.SplayHeap (Ch02_binary_search_trees.IntOrder))

let () =
  Alcotest.run "ch05 queues"
    [
      ("BatchedQueue", queue_tests);
      ("SplayHeap",    SHT.tests);
    ]
