module FifoOrder where

-- Single-threaded observable — wire-order preservation under sequential
-- send/receive only. Concurrent multi-producer *sending* is exercised
-- by 'CloneChain.fst' (three parallel 'fork @()' producers), but its
-- observable is sum-based, so it's order-tolerant — same caveat as
-- the Int-sum strategy rejected below. There is no current test that
-- proves FIFO receive ORDER under concurrent producers: that's a
-- future test-suite addition.
--
-- Four-message protocol; the writer sends 4, 3, 2, 1 in that order then
-- drops. The reader consumes them in FIFO wire-order and prints each
-- on its own line.
--
-- Order-preservation strategy:
--   * Receive sequence lives entirely in 'main' (no 'fork'). Receive
--     order on the channel = print order on stdout. Stdout is captured
--     line-by-line by ValidSpec, so a misordered receive would surface
--     as a different '.expected' line ordering.
--   * Each receiveA is followed by a print immediately, so the line
--     on stdout maps 1-to-1 to a single received integer. A reordering
--     of receiveA calls would reshuffle the printed integers.
--   * The fourth consume terminates the receiver continuation via
--     'waitA' (its protocol is at 'Wait').
--   * 'collectInts : -> ()' returns unit; the four 'NothingL -> ()'
--     arms return the same unit (the protocol guarantees delivery of
--     all four messages — these arms are statically dead, but accepting
--     them keeps 'case receiveA' syntactically exhaustive).
--
-- Why not other observables (per the migration report's documented
-- parser limitations):
--   * [Int] collect-list form: the FreeST parser rejects BOTH 0-arity
--     empty-list literals with type annotation (`[] @[Int]`) AND
--     prefix-cons expressions (`n : []`). See report §5.4.
--   * Int-sum form: doesn't prove order (permutations of [4,3,2,1] all
--     sum to 10), so the original test intent is lost.
type Four : 1S
type Four = !Int ; !Int ; !Int ; !Int ; Close

sendInts : **!Four -1-> ()
sendInts c =
  let c1 = sendA 4 c in
  let c2 = sendA 3 c1 in
  let c3 = sendA 2 c2 in
  let c4 = sendA 1 c3 in
  drop c4

collectInts : **?(?Int ; ?Int ; ?Int ; ?Int ; Wait) -1-> ()
collectInts c =
  case receiveA c of
    NothingL    -> ()
    JustL (n1, c1) ->
      print n1;
      case receiveA c1 of
        NothingL    -> ()
        JustL (n2, c2) ->
          print n2;
          case receiveA c2 of
            NothingL    -> ()
            JustL (n3, c3) ->
              print n3;
              case receiveA c3 of
                NothingL    -> ()
                JustL (n4, r4) ->
                  let _ = waitA r4 in
                  print n4


main : ()
main =
  let (rx, wx) = newA @Four () in
  sendInts wx;
  collectInts rx
