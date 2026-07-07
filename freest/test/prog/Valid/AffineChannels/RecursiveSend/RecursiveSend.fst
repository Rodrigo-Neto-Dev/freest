module RecursiveSend where

-- Five-message protocol. The writer sends 5,4,3,2,1 and drops.
--
-- Migration:
--   * 'sendDown' is linear because 'c5' is linear (the sender).
--   * Reader terminal arm binds linear continuation as 'r''''' and
--     terminates via 'waitA'.
--   * The fork thunk is linear because it consumes the linear
--     sender 's' — using an unrestricted thunk would discard 's' on
--     function-return without consuming it.
type Five : 1S
type Five = !Int ; !Int ; !Int ; !Int ; !Int ; Close

sendDown : **!Five -1-> ()
sendDown c =
  let c1 = sendA 5 c in
  let c2 = sendA 4 c1 in
  let c3 = sendA 3 c2 in
  let c4 = sendA 2 c3 in
  let c5 = sendA 1 c4 in
  drop c5

collectAll : **?(?Int ; ?Int ; ?Int ; ?Int ; ?Int ; Wait) -1-> ()
collectAll r =
  case receiveA r of
    NothingL -> ()
    JustL (a, r') ->
      print a;
      case receiveA r' of
        NothingL -> ()
        JustL (b, r'') ->
          print b;
          case receiveA r'' of
            NothingL -> ()
            JustL (c, r''') ->
              print c;
              case receiveA r''' of
                NothingL -> ()
                JustL (d, r'''') ->
                  print d;
                  case receiveA r'''' of
                    NothingL         -> ()
                    JustL (e, r''''') ->
                      let () = waitA r''''' in
                      print e

main : ()
main =
  -- Prelude newA signature: tuple is (receiver, sender).
  let (r, s) = newA @Five () in
  -- 'sendDown' consumes 's'; ran inside a linear fork so the main thread
  -- can still observe 'collectAll r' cleanly.
  fork @() (\_ -1-> sendDown s);
  collectAll r
