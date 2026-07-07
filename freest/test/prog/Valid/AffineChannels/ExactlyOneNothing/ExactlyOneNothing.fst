module ExactlyOneNothing where

-- The writer protocol expects two messages but the writer only sends one
-- before it drops mid-stream. The reader, expecting two, sees JustL on
-- the first and NothingL on the second — exactly one Nothing.
--
-- Migration: avoid 'countNothing : **?TwoRecv -1-> Int -> Int' (the
-- linearity checker rejected the body as "unrestricted function consuming
-- a linear var"). Inline the receive/count into main; bind the linear
-- continuation as 'c1' / 'c2' and consume the terminal Wait via waitA.
type TwoSend : 1S
type TwoSend = !Int ; !Int ; Close

type TwoRecv : 1S
type TwoRecv = ?Int ; ?Int ; Wait

main : ()
main =
  -- Prelude newA signature: tuple is (receiver, sender).
  let (r, s) = newA @TwoSend () in
  fork @() (\_ -1->
    let s1 = sendA 42 s in
    drop s1);
  -- Inline: count the Nothings (expect exactly one).
  -- Expected total: 1 (one Nothing from the writer dropping mid-stream).
  case receiveA r of
    NothingL    -> print 1
    JustL (_, r1) ->
      case receiveA r1 of
        NothingL    -> print 1   -- exactly one Nothing observed
        JustL (_, r2) ->
          let () = waitA r2 in
          print 0                -- protocol was completed without further Nothings
