module PatternMatchReceive where

sumInts : **?Int -> Int
sumInts c =
  case receiveA c of
    Nothing     -> 0
    Just (n, c) -> n + sumInts c

main : ()
main =
  let (rx, wx) = new @**!Int () in
  sendA 10 wx ;
  sendA 20 wx ;
  sendA 30 wx ;
  drop wx ;
  print (sumInts rx)