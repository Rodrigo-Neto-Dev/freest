module BranchMissingDrop where

-- Error: 'wx' is consumed only in the 'then' branch; the 'else'
-- branch leaves it unconsumed, breaking linear scope.
type One : 1S
type One = !Int ; Close

main : ()
main =
  let (rx, wx) = newA @One () in
  let b = True in
  if b
  then
    let _ = sendA 1 wx in
    drop wx
  else ();
  case receiveA rx of
    NothingL    -> print 0
    JustL (n, _) -> print n
