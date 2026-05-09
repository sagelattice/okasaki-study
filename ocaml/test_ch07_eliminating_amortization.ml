open Okasaki
open Okasaki_test_helpers.Queue_test_helpers

module RTQ = QueueTests (Ch07_eliminating_amortization.RealTimeQueue (Ch04_streams.Stream))

let () =
  Alcotest.run "ch07"
    [
      ("RealTimeQueue", RTQ.tests);
    ]
