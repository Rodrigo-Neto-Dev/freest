module InterleavingSumCheck where

-- Three-message writer protocol, with the sender cloned three ways. Each
-- forked producer sends its own int and drops; the reader sums 1+5+9 = 15
-- (regardless of interleaving).
--
-- Migration: avoid helper-functions that take a linear 'recv' alongside an
-- unrestricted Int accumulator (the linearity checker rejects the body as
-- "unrestricted function consuming a linear var"). Inline the receive/sum
-- into main; each JustL arm binds the linear continuation as 'r_i' and
-- the terminal Wait continuation ('r_i') is consumed by 'waitA'.
type Three : 1S
type Three = !Int ; !Int ; !Int ; Close

producer : Int -> **!Three -1-> ()
producer x s = drop (sendA x s)

main : ()
main =
  -- Prelude newA signature: tuple is (receiver, sender).
  let (r, s)    = newA @Three () in
  let (s1, s2)  = cloneAS s in
  let (s2a, s2b) = cloneAS s2 in
  fork @() (\_ -1-> producer 1 s1);
  fork @() (\_ -1-> producer 5 s2a);
  fork @() (\_ -1-> producer 9 s2b);
  case receiveA r of
    NothingL    -> print 0
    JustL (x, r1) ->
      case receiveA r1 of
        NothingL    -> print x
        JustL (y, r2) ->
          case receiveA r2 of
            NothingL    -> print (x + y)
            JustL (z, r3) ->
              let () = waitA r3 in
              print (x + y + z)
