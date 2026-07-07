module CloneReceiver where

-- Error: 'cloneAS' expects an AffineSender (**!s) but is being
-- applied to an AffineReceiver ('rx : **?(?Int; Wait)').
type One : 1S
type One = !Int ; Close

main : ()
main =
  let (rx, wx) = newA @One () in
  drop wx;
  let (rx1, rx2) = cloneAS rx in
  case receiveA rx1 of
    NothingL    -> print 0
    JustL (_, _) -> ()
