open Okasaki
open Okasaki_test_helpers.Heap_test_helpers

module LHT = HeapTests (Ch03_heaps.LeftistHeap (Ch02_binary_search_trees.IntOrder))
module BHT = HeapTests (Ch03_heaps.BinomialHeap (Ch02_binary_search_trees.IntOrder))

let () =
  Alcotest.run "ch03 heaps"
    [
      ("LeftistHeap",  LHT.tests);
      ("BinomialHeap", BHT.tests);
    ]
