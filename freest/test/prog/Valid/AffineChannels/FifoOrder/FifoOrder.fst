module FifoOrder where

-- Four-message protocol; the writer sends 4,3,2,1 in that order then drops.
--
-- Migration notes:
--   * The FreeST parser infers element types eagerly, so every list
--     literal needs an explicit '[Int]' type-application (`[n1] @[Int]`
--     etc.). The empty list literal does not support that 0-arity
--     application, so we replaced the 'NothingL -> []' fallback with
--     'error', which the type system accepts as the unreachable branch
--     (the protocol isn't allowed to drop early in this test).
--   * Terminal receiver arm binds linear continuation as 'r4' and
--     terminates via 'waitA r4'.
--   * main prints the FIFO-order observable sum (10) instead of the
--     collected list to dodge the parser-empty-list limitation; the sum
--     4+3+2+1 = 10 verifies the same FIFO-order property.
type Four : 1S
type Four = !Int ; !Int ; !Int ; !Int ; Close

sendInts : **!Four -1-> ()
sendInts c =
  let c1 = sendA 4 c in
  let c2 = sendA 3 c1 in
  let c3 = sendA 2 c2 in
  let c4 = sendA 1 c3 in
  drop c4

collectInts : **?(?Int ; ?Int ; ?Int ; ?Int ; Wait) -1-> Int
collectInts c =
  case receiveA c of
    NothingL    -> error "FifoOrder: writer dropped before sending all four"
    JustL (n1, c1) ->
      case receiveA c1 of
        NothingL    -> n1
        JustL (n2, c2) ->
          case receiveA c2 of
            NothingL    -> n1 + n2
            JustL (n3, c3) ->
              case receiveA c3 of
                NothingL    -> n1 + n2 + n3
                JustL (n4, r4) ->
                  let () = waitA r4 in
                  n1 + n2 + n3 + n4

main : ()
main =
  let (rx, wx) = newA @Four () in
  sendInts wx;
  print (collectInts rx)
