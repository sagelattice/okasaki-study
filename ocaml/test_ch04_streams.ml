open Okasaki

let stream_tests =
  let xs = [ 1; 2; 3 ] in
  let s = Ch04_streams.Stream.of_list xs in
  let open Ch04_streams.Stream in
  Alcotest.
    [
      (* of_list and to_list are inverses *)
      test_case "of_list / to_list round-trip" `Quick (fun () ->
        Alcotest.(check (list int)) "to_list (of_list xs) = xs" xs (to_list s));

      (* drop removes the requested number of elements from the front *)
      test_case "drop 1" `Quick (fun () ->
        Alcotest.(check (list int)) "drop 1 s = [2;3]"
          (List.drop 1 xs) (to_list (drop 1 s)));

      (* dropping more elements than exist yields the empty stream *)
      test_case "drop past end" `Quick (fun () ->
        Alcotest.(check (list int)) "drop 42 s = []" [] (to_list (drop 42 s)));

      (* drop 0 is identity *)
      test_case "drop 0 identity" `Quick (fun () ->
        Alcotest.(check (list int)) "drop 0 s = xs"
          (List.drop 0 xs) (to_list (drop 0 s)));

      (* take returns the requested number of elements from the front *)
      test_case "take 1" `Quick (fun () ->
        Alcotest.(check (list int)) "take 1 s = [1]"
          (List.take 1 xs) (to_list (take 1 s)));

      (* taking more elements than exist yields the whole stream *)
      test_case "take past end" `Quick (fun () ->
        Alcotest.(check (list int)) "take 42 s = xs" xs (to_list (take 42 s)));

      (* take 0 yields the empty stream *)
      test_case "take 0 empty" `Quick (fun () ->
        Alcotest.(check (list int)) "take 0 s = []"
          (List.take 0 xs) (to_list (take 0 s)));

      (* reverse produces elements in reverse order *)
      test_case "reverse" `Quick (fun () ->
        Alcotest.(check (list int)) "reverse [1;2;3] = [3;2;1]"
          (List.rev xs) (to_list (reverse s)));

      (* ++ concatenates two finite streams *)
      test_case "concat non-empty" `Quick (fun () ->
        Alcotest.(check (list int)) "s ++ [4] = [1;2;3;4]"
          [ 1; 2; 3; 4 ] (to_list (s ++ of_list [ 4 ])));

      (* appending the empty stream on the right is identity *)
      test_case "concat right identity" `Quick (fun () ->
        Alcotest.(check (list int)) "s ++ Nil = xs" xs
          (to_list (s ++ lazy Nil)));

      (* appending the empty stream on the left is identity *)
      test_case "concat left identity" `Quick (fun () ->
        Alcotest.(check (list int)) "Nil ++ s = xs" xs
          (to_list (lazy Nil ++ s)));
    ]

let () =
  Alcotest.run "ch04 streams"
    [
      ("Stream", stream_tests);
    ]
