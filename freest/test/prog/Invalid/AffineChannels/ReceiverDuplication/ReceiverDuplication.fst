module ReceiverUsedDuplication where

sumInts : **?Int -> Int
sumInts c = case receiveA c of
  Nothing     -> 0
  Just (n, c) -> n + sumInts c

main : ()
main =
  let (rx, wx) = new @**!Int () in
  drop wx ;
  let _ = sumInts rx in
  let _ = sumInts rx in  -- Error: rx not in scope
  ()