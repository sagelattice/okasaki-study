open Okasaki
open Okasaki_test_helpers.Queue_test_helpers
open Okasaki_test_helpers.Heap_test_helpers

module RTQ = QueueTests (Ch07_eliminating_amortization.RealTimeQueue (Ch04_streams.Stream))
module SBH = HeapTests (Ch07_eliminating_amortization.ScheduledBinomialHeap (Ch02_binary_search_trees.IntOrder) (Ch04_streams.Stream))

let () =
  Alcotest.run "ch07"
    [
      ("RealTimeQueue",           RTQ.tests);
      ("ScheduledBinomialHeap",   SBH.tests);
    ]
