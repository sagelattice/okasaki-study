open Okasaki
open Okasaki_test_helpers.Queue_test_helpers
module HMQ = QueueTests (Ch08_lazy_rebuilding.HoodMelvilleQueue)

module C3 : Sigs.CONST_INT = struct
  let c = 3
end

module BDQ =
  DequeTests (Ch08_lazy_rebuilding.BankersDeque (C3) (Ch04_streams.Stream))

let () =
  Alcotest.run "ch08"
    [ ("HoodMelvilleQueue", HMQ.tests); ("BankersDeque", BDQ.tests) ]
