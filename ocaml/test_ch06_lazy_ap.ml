open Okasaki
open Okasaki_test_helpers.Queue_test_helpers
open Okasaki_test_helpers.Heap_test_helpers
open Okasaki_test_helpers.Sort_test_helpers

module BKT = QueueTests (Ch06_lazy_ap.BankersQueue (Ch04_streams.Stream))
module LBH = HeapTests (Ch06_lazy_ap.LazyBinomialHeap (Ch02_binary_search_trees.IntOrder))
module PHQ = QueueTests (Ch06_lazy_ap.PhysicistsQueue)
module BUM = SortableTests (Ch06_lazy_ap.BottomUpMergeSort (Ch02_binary_search_trees.IntOrder))

let () =
  Alcotest.run "ch06"
    [
      ("BankersQueue",      BKT.tests);
      ("LazyBinomialHeap",  LBH.tests);
      ("PhysicistsQueue",   PHQ.tests);
      ("BottomUpMergeSort", BUM.tests);
    ]
