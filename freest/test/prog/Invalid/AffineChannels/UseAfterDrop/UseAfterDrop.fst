module UseAfterDrop where

-- Error: 'wx' has been dropped and so a subsequent 'sendA' has no
-- referent.
type One : 1S
type One = !Int ; Close

main : ()
main =
  let (rx, wx) = newA @One () in
  drop wx;
  sendA 1 wx   -- error: wx already consumed
