module CloneChain where

sendOne : Int -> **!Int 1-> ()
sendOne x c =
  drop (sendA x c)

sumInts : **?Int 1-> Int
sumInts c = case receiveA c of
  Nothing     -> 0
  Just (n, c) -> n + sumInts c

main : ()
main =
  let (rx, wx) = newA @Int () in
  let (wx1, wxR)  = cloneAS wx  in
  let (wx2, wx3)  = cloneAS wxR in
  fork (\_ -> sendOne 1 wx1) ;
  fork (\_ -> sendOne 2 wx2) ;
  fork (\_ -> sendOne 3 wx3) ;
  print (sumInts rx)