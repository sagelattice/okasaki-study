open Okasaki

module SortableTests (S : Sigs.SORTABLE with type Elem.t = int) = struct
  open S

  let build xs = List.fold_left (fun s x -> add x s) empty xs

  let test_empty_sorts_to_empty () =
    Alcotest.(check (list int)) "sort empty = []" [] (sort empty)

  let test_singleton () =
    Alcotest.(check (list int)) "sort [1] = [1]" [ 1 ] (sort (add 1 empty))

  let test_unsorted_input () =
    Alcotest.(check (list int)) "sort [3;1;2] = [1;2;3]" [ 1; 2; 3 ]
      (sort (build [ 3; 1; 2 ]))

  let test_already_sorted () =
    Alcotest.(check (list int)) "sort [1;2;3] = [1;2;3]" [ 1; 2; 3 ]
      (sort (build [ 1; 2; 3 ]))

  let test_reverse_sorted () =
    Alcotest.(check (list int)) "sort [5;4;3;2;1] = [1;2;3;4;5]" [ 1; 2; 3; 4; 5 ]
      (sort (build [ 5; 4; 3; 2; 1 ]))

  let test_duplicates () =
    Alcotest.(check (list int)) "sort [3;1;2;1;3] = [1;1;2;3;3]" [ 1; 1; 2; 3; 3 ]
      (sort (build [ 3; 1; 2; 1; 3 ]))

  let test_all_equal () =
    Alcotest.(check (list int)) "sort [2;2;2] = [2;2;2]" [ 2; 2; 2 ]
      (sort (build [ 2; 2; 2 ]))

  let test_large_input () =
    let xs = List.init 100 (fun i -> 99 - i) in
    let expected = List.init 100 (fun i -> i) in
    Alcotest.(check (list int)) "sort 99..0 = 0..99" expected (sort (build xs))

  let test_immutability () =
    let s = build [ 3; 1; 2 ] in
    ignore (add 0 s);
    Alcotest.(check (list int)) "s unchanged after add 0" [ 1; 2; 3 ] (sort s)

  let test_incremental_add () =
    let s1 = build [ 3; 1 ] in
    let s2 = add 2 s1 in
    Alcotest.(check (list int)) "s1 still [1;3]" [ 1; 3 ] (sort s1);
    Alcotest.(check (list int)) "s2 is [1;2;3]" [ 1; 2; 3 ] (sort s2)

  let tests =
    Alcotest.
      [
        test_case "sort empty"      `Quick test_empty_sorts_to_empty;
        test_case "singleton"       `Quick test_singleton;
        test_case "unsorted input"  `Quick test_unsorted_input;
        test_case "already sorted"  `Quick test_already_sorted;
        test_case "reverse sorted"  `Quick test_reverse_sorted;
        test_case "duplicates"      `Quick test_duplicates;
        test_case "all equal"       `Quick test_all_equal;
        test_case "large input"     `Quick test_large_input;
        test_case "immutability"    `Quick test_immutability;
        test_case "incremental add" `Quick test_incremental_add;
      ]
end
