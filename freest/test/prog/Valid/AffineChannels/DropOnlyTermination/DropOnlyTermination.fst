module DropOnlyTermination where

-- Writer drops immediately without sending anything; the runtime EOF
-- token arrives on the first receiveA.
--
-- Migration: the unreachable 'JustL' arm now binds the linear receiver
-- continuation '**?Wait' as 'r1' and terminates via 'waitA r1' instead
-- of discarding via '_'.
main : ()
main =
  let (rx, wx) = newA @(!Int; Close) () in
  drop wx;
  case receiveA rx of
    NothingL    -> print "closed"
    JustL (_, r1) ->
      let () = waitA r1 in
      ()
