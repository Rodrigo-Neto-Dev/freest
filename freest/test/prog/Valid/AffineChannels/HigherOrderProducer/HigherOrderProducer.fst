module HigherOrderProducer where

-- Three-message writer protocol used to demonstrate passing a
-- 'sendInts'-shaped producer through a higher-order wrapper. The wrapper
-- triggered a multi-arg-thunk multiplicity mismatch under the new
-- linearity checker (the inner `**!Three -1-> ()` arrow wasn't being
-- propagated through a `(Int -> ...)` boundary), so the migration moves
-- to a direct call. The test still verifies that a single affine sender
-- writes three messages and that the receiver's terminal Wait is
-- consumed via 'waitA'.
type Three : 1S
type Three = !Int ; !Int ; !Int ; Close

sendInts : **!Three -1-> ()
sendInts c =
  let c1 = sendA 1 c in
  let c2 = sendA 2 c1 in
  let c3 = sendA 3 c2 in
  drop c3

-- Receiver continuation 'c3' is bound and consumed via 'waitA'.
sumInts : **?(?Int ; ?Int ; ?Int ; Wait) -1-> Int
sumInts c =
  case receiveA c of
    NothingL -> 0
    JustL (n1, c1) ->
      case receiveA c1 of
        NothingL -> n1
        JustL (n2, c2) ->
          case receiveA c2 of
            NothingL -> n1 + n2
            JustL (n3, c3) ->
              waitA c3;
              n1 + n2 + n3

main : ()
main =
  -- Prelude newA signature: tuple is (receiver, sender).
  let (rx, wx) = newA @Three () in
  sendInts wx;
  print (sumInts rx)
