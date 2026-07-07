module TwoProducers where

-- Six-message protocol. Both producer clones send 1,2,3 and drop.
-- Order between forks is nondeterministic, but the reader sums each
-- first int (1), second (2), third (3), etc. so the total is
-- 1+2+3+1+2+3 = 12 regardless of ordering.
--
-- Migration:
--   * Reader terminal arm binds linear continuation as 'c6' and
--     terminates via 'waitA'.
--   * Fork thunks are linear ('-1->') because they capture linear sender
--     clones.
--   * Module-level sendInts was inline'd in the forks because FreeST's
--     name-lookup didn't bring module-level names into the fork-thunk
--     scope under the new linearity checker — the let-sendA chain is
--     now written directly.
type Six : 1S
type Six = !Int ; !Int ; !Int ; !Int ; !Int ; !Int ; Close

-- Explicit type signature required (the original made this inferred,
-- which the FreeST compiler now rejects under strict-signature enforcement).
sumInts : **?(?Int ; ?Int ; ?Int ; ?Int ; ?Int ; ?Int ; Wait) -1-> Int
sumInts c =
  case receiveA c of
    NothingL -> 0
    JustL (n1, c1) ->
      case receiveA c1 of
        NothingL -> n1
        JustL (n2, c2) ->
          case receiveA c2 of
            NothingL -> n1 + n2
            JustL (n3, c3) ->
              case receiveA c3 of
                NothingL -> n1 + n2 + n3
                JustL (n4, c4) ->
                  case receiveA c4 of
                    NothingL -> n1 + n2 + n3 + n4
                    JustL (n5, c5) ->
                      case receiveA c5 of
                        NothingL -> n1 + n2 + n3 + n4 + n5
                        JustL (n6, c6) ->
                          let () = waitA c6 in
                          n1 + n2 + n3 + n4 + n5 + n6

main : ()
main =
  -- Prelude newA signature: tuple is (receiver, sender).
  let (rx, wx)  = newA @Six () in
  let (wx1, wx2) = cloneAS wx in
  -- Inline the 6-send-and-drop inside each fork thunk.
  fork @() (\_ -1->
    drop (sendA 3 (sendA 2 (sendA 1 (sendA 1 (sendA 2 (sendA 3 wx1)) )))));
  fork @() (\_ -1->
    drop (sendA 3 (sendA 2 (sendA 1 (sendA 1 (sendA 2 (sendA 3 wx2)) )))));
  print (sumInts rx)
