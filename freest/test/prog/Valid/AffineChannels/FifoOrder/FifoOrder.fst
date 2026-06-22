module FifoOrder where

sendInts : Int -> **!Int 1-> ()
sendInts 0 c = drop c
sendInts n c =
  sendInts (n - 1) (sendA n c)

collectInts : **?Int 1-> [Int]
collectInts c = case receiveA c of
  Nothing     -> []
  Just (n, c) -> n : collectInts c

main : ()
main =
  let (rx, wx) = newA @Int () in
  sendInts 4 wx ;
  print (collectInts rx)