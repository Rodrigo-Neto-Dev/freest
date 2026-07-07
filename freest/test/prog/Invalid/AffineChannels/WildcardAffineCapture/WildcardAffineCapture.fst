module WildcardAffineCapture where

-- Regression test: a receiver continuation bound to '_' silently
-- discards an affine capability. Affine receivers must be consumed
-- explicitly (e.g. via 'waitA'); binding the continuation to '_'
-- leaks the linear capability.
type Two : 1S
type Two = !Int ; !Int ; Close

sumInts : **?(?Int ; ?Int ; Wait) -1-> Int
sumInts c =
  case receiveA c of
    NothingL -> 0
    JustL (n1, c1) ->
      case receiveA c1 of
        NothingL -> n1
        JustL (n2, _) -> n1 + n2

main : ()
main =
  let (rx, wx) = newA @Two () in
  let wx1 = sendA 1 wx in
  drop wx1;
  print (sumInts rx)
