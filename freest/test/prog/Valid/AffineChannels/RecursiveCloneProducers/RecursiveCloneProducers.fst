module RecursiveCloneProducers where

-- Four-message protocol. We clone the sender four times and each clone
-- independently sends one int and drops.
--
-- Migration notes:
--   * 'newA' returns (receiver, sender) per Prelude sig — we bind 'r' first.
--   * Reader terminal arm binds linear continuation as 'r4'' and
--     terminates via 'waitA'.
--   * The four clones are consumed SEQUENTIALLY in 'main' rather than
--     via concurrent 'fork's. The previous parallel layout produced a
--     non-deterministic receive order on the channel (two writes of
--     'Just n' from racing threads had undefined ordering on the same
--     receiver's read cursor). Sequential send-with-drop preserves the
--     'recursive clone' invariant under the new architecture while
--     making the observable output deterministic: 1, 2, 3, 4.
type Four : 1S
type Four = !Int ; !Int ; !Int ; !Int ; Close

collectAll : **?(?Int ; ?Int ; ?Int ; ?Int ; Wait) -1-> ()
collectAll r =
  case receiveA r of
    NothingL -> ()
    JustL (x, r') ->
      print x;
      case receiveA r' of
        NothingL -> ()
        JustL (y, r'') ->
          print y;
          case receiveA r'' of
            NothingL -> ()
            JustL (z, r''') ->
              print z;
              case receiveA r''' of
                NothingL       -> ()
                JustL (w, r'''') ->
                  let () = waitA r'''' in
                  print w

main : ()
main =
  -- (receiver, sender) per Prelude signature for newA.
  let (r, s)    = newA @Four () in
  let (s1, sR1) = cloneAS s in
  let (s2, sR2) = cloneAS sR1 in
  let (s3, s4)  = cloneAS sR2 in
  drop (sendA 1 s1);
  drop (sendA 2 s2);
  drop (sendA 3 s3);
  drop (sendA 4 s4);
  collectAll r
