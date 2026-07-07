module RecursiveCloneProducers where

-- Four-message protocol. We clone the sender four times (sequentially)
-- and each clone independently sends one int and drops.
--
-- Migration:
--   * 'newA' returns (receiver, sender) per Prelude sig — we bind 'r'
--     first.
--   * Reader terminal arm binds linear continuation as 'r'''' and
--     terminates via 'waitA'.
--   * Fork thunks are linear ('-1->') because each captures a linear
--     sender clone.
--   * 'cloneAS' is linear (multiplicity 1) because it consumes and
--     returns linear senders.
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
  fork @() (\_ -1-> drop (sendA 1 s1));
  fork @() (\_ -1-> drop (sendA 2 s2));
  fork @() (\_ -1-> drop (sendA 3 s3));
  fork @() (\_ -1-> drop (sendA 4 s4));
  collectAll r
