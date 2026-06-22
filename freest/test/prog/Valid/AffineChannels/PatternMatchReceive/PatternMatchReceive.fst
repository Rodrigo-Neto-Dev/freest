module PatternMatchReceive where

sumInts : **?Int 1-> Int
sumInts c =
  case receiveA c of
    Nothing     -> 0
    Just (n, c) -> n + sumInts c

main : ()
main =
  let (rx, wx) = newA @Int () in
  let wx1 = sendA 10 wx in
  let wx2 = sendA 20 wx1 in
  drop (sendA 30 wx2) ;
  print (sumInts rx)