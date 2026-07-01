module SendInts where

main : ()
main =
  let (r, w) = newA @Int () in
  fork (\_ -> drop w);
  print (sumInts r)

sumInts : **?Int -1-> Int
sumInts c = case receiveA c of
  NothingL    -> 0
  JustL (n, c) -> n + sumInts c