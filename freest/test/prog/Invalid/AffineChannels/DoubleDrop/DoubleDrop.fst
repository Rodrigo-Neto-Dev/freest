module DoubleDrop where

-- Error: 'wx' has been dropped and so is no longer in scope.
type One : 1S
type One = !Int ; Close

main : ()
main =
  let (rx, wx) = newA @One () in
  drop wx;
  drop wx   -- error: wx already consumed
