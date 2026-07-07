module SingleProducerTermination where

-- Three-message protocol. The writer sends 1, 2, 3 and drops; the reader
-- consumes each with the literal JustL pattern and prints each value, and
-- the third receive's continuation '**?Wait' is bound to 'r3' so we can
-- terminate it explicitly via 'waitA' rather than (a) attempting another
-- 'receiveA' on a Wait-typed channel, or (b) discarding via '_'.
main : ()
main =
  -- Prelude newA signature: tuple is (receiver, sender).
  let (r, s) = newA @(!Int; !Int; !Int; Close) () in
  let s1 = sendA 1 s in
  let s2 = sendA 2 s1 in
  let s3 = sendA 3 s2 in
  drop s3;
  case receiveA r of
    NothingL    -> ()
    JustL (x, r1) ->
      print x;
      case receiveA r1 of
        NothingL    -> ()
        JustL (y, r2) ->
          print y;
          case receiveA r2 of
            NothingL    -> ()
            JustL (z, r3) ->
              print z;
              let () = waitA r3 in
              print "done"
