module HigherOrderProducer where

withSender : (Int -> **!Int 1-> ()) -> Int -> **!Int 1-> ()
withSender f n c = f n c

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
  withSender sendInts 3 wx ;
  print (sumInts rx)