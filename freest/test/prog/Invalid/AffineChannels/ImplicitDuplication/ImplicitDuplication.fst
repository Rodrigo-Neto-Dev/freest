module ImplicitDuplication where

-- Error: 'wx' is passed to two forks without being cloned, so it
-- cannot be used by both threads.
type One : 1S
type One = !Int ; Close

main : ()
main =
  let (rx, wx) = newA @One () in
  fork (\_ -> drop (sendA 1 wx));
  fork (\_ -> drop (sendA 2 wx));
  case receiveA rx of
    NothingL    -> print 0
    JustL (n, _) -> print n
