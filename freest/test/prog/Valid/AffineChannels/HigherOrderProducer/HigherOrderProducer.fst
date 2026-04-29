module HigherOrderProducer where

withSender : (Int -> **!Int -> ()) -> Int -> **!Int -> ()
withSender f n c = f n c

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
  withSender sendInts 3 wx ;
  print (sumInts rx)