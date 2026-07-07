module ReceiverNotConsumed where

type One : 1S
type One = !Int ; Close

-- Error: 'rx' goes out of scope without being consumed.
main : ()
main =
  let (rx, wx) = newA @One () in
  drop wx
