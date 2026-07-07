module ReceiverDuplication where

-- Error: 'rx' is used twice in body without cloning first.
type One : 1S
type One = !Int ; Close

sumInts : **?(?Int ; Wait) -1-> Int
sumInts r =
  case receiveA r of
    NothingL -> 0
    JustL (n, _) -> n

main : ()
main =
  let (rx, wx) = newA @One () in
  drop wx;
  let _ = sumInts rx in
  let _ = sumInts rx in
  print 0
