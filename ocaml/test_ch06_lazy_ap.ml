open Okasaki
open Okasaki_test_helpers.Queue_test_helpers

module BKT = QueueTests (Ch06_lazy_ap.BankersQueue (Ch04_streams.Stream))

let () =
  Alcotest.run "ch06"
    [
      ("BankersQueue", BKT.tests);
    ]
