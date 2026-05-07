open Okasaki
open Okasaki_test_helpers.Queue_test_helpers

module BKT = QueueTests (Ch06_lazy_ap.BankersQueue)

let () =
  Alcotest.run "ch06"
    [
      ("BankersQueue", BKT.tests);
    ]
