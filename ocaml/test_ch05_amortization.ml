open Okasaki
open Okasaki_test_helpers.Heap_test_helpers
open Okasaki_test_helpers.Queue_test_helpers

module BQT = QueueTests (Ch05_amortization.BatchedQueue)
module SHT = HeapTests (Ch05_amortization.SplayHeap (Ch02_binary_search_trees.IntOrder))
module PHT = HeapTests (Ch05_amortization.PairingHeap (Ch02_binary_search_trees.IntOrder))

let () =
  Alcotest.run "ch05"
    [
      ("BatchedQueue", BQT.tests);
      ("SplayHeap",    SHT.tests);
      ("PairingHeap",  PHT.tests);
    ]
