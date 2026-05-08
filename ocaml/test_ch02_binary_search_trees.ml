open Okasaki
open Okasaki_test_helpers.Set_test_helpers

module UST =
  SetTests
    (Ch02_binary_search_trees.UnbalancedSet (Ch02_binary_search_trees.IntOrder))

let () =
  Alcotest.run "ch02 binary search trees"
    [
      ("UnbalancedSet", UST.tests);
    ]
