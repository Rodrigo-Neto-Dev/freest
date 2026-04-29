module TwoProducers where

sendInts : Int -> **!Int -> ()
sendInts 0 c = drop c
sendInts n c =
  sendA n c ;
  sendInts (n - 1) c

sumInts : **?Int -> Int
sumInts c = case receiveA c of
  Nothing     -> 0
  Just (n, c) -> n + sumInts c

main : ()
main =
  let (rx, wx) = new @**!Int () in
  let (wx1, wx2) = clone wx in
  fork (\_ -> sendInts 3 wx1) ;
  fork (\_ -> sendInts 3 wx2) ;
  print (sumInts rx)