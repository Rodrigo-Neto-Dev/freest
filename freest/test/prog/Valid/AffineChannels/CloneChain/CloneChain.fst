module CloneChain where

sendOne : Int -> **!Int -> ()
sendOne x c =
  sendA x c ;
  drop c

sumInts : **?Int -> Int
sumInts c = case receiveA c of
  Nothing     -> 0
  Just (n, c) -> n + sumInts c

main : ()
main =
  let (rx, wx) = new @**!Int () in
  let (wx1, wxR)  = clone wx  in
  let (wx2, wx3)  = clone wxR in
  fork (\_ -> sendOne 1 wx1) ;
  fork (\_ -> sendOne 2 wx2) ;
  fork (\_ -> sendOne 3 wx3) ;
  print (sumInts rx)