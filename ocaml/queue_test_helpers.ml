open Okasaki

module QueueTests (Q : Ch05_amortization.QUEUE) = struct
  open Q

  let to_list q =
    let rec go acc q =
      if is_empty q then List.rev acc
      else go (head q :: acc) (tail q)
    in
    go [] q

  let build xs = List.fold_left snoc empty xs

  let tests =
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
          Alcotest.check_raises "head [] raises" EMPTY
            (fun () -> ignore (head empty)));

        test_case "tail raises on empty" `Quick (fun () ->
          Alcotest.check_raises "tail [] raises" EMPTY
            (fun () -> ignore (tail empty)));

        test_case "tail drain" `Quick (fun () ->
          let q = build [ 1; 2; 3 ] in
          let q = tail (tail q) in
          Alcotest.(check (list int)) "remaining element after two tails"
            [ 3 ] (to_list q));

        test_case "large queue round-trip" `Quick (fun () ->
          let xs = List.init 100 (fun i -> i) in
          Alcotest.(check (list int)) "100-element round-trip" xs
            (to_list (build xs)));

        test_case "tail: branch independence" `Quick (fun () ->
          let q = build [ 1; 2; 3; 4; 5 ] in
          let b1 = tail q in
          let b2 = tail q in
          Alcotest.(check int) "head (tail q) = 2" 2 (head b1);
          Alcotest.(check int) "head (tail q) = 2 again" 2 (head b2);
          Alcotest.(check int) "head (tail (tail q)) = 3" 3 (head (tail b1));
          Alcotest.(check int) "head (tail (tail q)) = 3 again" 3 (head (tail b2)));

        test_case "snoc: branches from shared base are independent" `Quick (fun () ->
          let q = build [ 1; 2; 3 ] in
          let q1 = snoc q 10 in
          let q2 = snoc q 20 in
          Alcotest.(check (list int)) "to_list (snoc q 10) = [1;2;3;10]"
            [ 1; 2; 3; 10 ] (to_list q1);
          Alcotest.(check (list int)) "to_list (snoc q 20) = [1;2;3;20]"
            [ 1; 2; 3; 20 ] (to_list q2);
          Alcotest.(check (list int)) "to_list q = [1;2;3]"
            [ 1; 2; 3 ] (to_list q));

        test_case "sliding window: snoc/tail interleaving preserves FIFO" `Quick (fun () ->
          let q = build [ 1; 2; 3; 4; 5 ] in
          let q = List.fold_left (fun q x -> snoc (tail q) x) q [ 6; 7; 8; 9; 10 ] in
          Alcotest.(check (list int)) "to_list q = [6;7;8;9;10]"
            [ 6; 7; 8; 9; 10 ] (to_list q));
      ]
end
