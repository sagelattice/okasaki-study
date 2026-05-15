open Okasaki
open Okasaki_test_helpers.Queue_test_helpers

module HMQ = QueueTests (Ch08_lazy_rebuilding.HoodMelvilleQueue)

let () =
  Alcotest.run "ch08"
    [
      ("HoodMelvilleQueue", HMQ.tests);
    ]
