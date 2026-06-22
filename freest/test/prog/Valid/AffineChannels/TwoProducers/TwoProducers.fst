module TwoProducers where

sendInts : Int -> **!Int 1-> ()
sendInts 0 c = drop c
sendInts n c =
  sendInts (n - 1) (sendA n c)

sumInts : **?Int 1-> Int
sumInts c = case receiveA c of
  Nothing     -> 0
  Just (n, c) -> n + sumInts c

main : ()
main =
  let (rx, wx) = newA @Int () in
  let (wx1, wx2) = cloneAS wx in
  fork (\_ -> sendInts 3 wx1) ;
  fork (\_ -> sendInts 3 wx2) ;
  print (sumInts rx)