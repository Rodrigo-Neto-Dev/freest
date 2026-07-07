module MinimalTest where

main : ()
main =
-- Minimal sanity check: one-shot protocol. The writer side ends with
-- 'Close'; the writer satisfies it with 'drop'. From the reader side the
-- first 'receiveA' peels the ?Int and yields 'JustL n'.
  let (r, w) = newA @(!Int; Close) () in
  let w1 = sendA 42 w in
  let w2 = drop w1 in
  case receiveA r of
    NothingL    -> print 0
    JustL (n, r1) ->
      let () = waitA r1 in
      print n