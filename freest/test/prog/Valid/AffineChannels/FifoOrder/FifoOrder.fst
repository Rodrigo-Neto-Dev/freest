module FifoOrder where

sendInts : Int -> **!Int -> ()
sendInts 0 c = drop c
sendInts n c =
  sendA n c ;
  sendInts (n - 1) c

collectInts : **?Int -> [Int]
collectInts c = case receiveA c of
  Nothing     -> []
  Just (n, c) -> n : collectInts c

main : ()
main =
  let (rx, wx) = new @**!Int () in
  sendInts 4 wx ;
  print (collectInts rx)