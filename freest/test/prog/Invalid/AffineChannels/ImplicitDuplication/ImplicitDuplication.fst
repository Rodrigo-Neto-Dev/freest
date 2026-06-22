module ImplicitDuplication where

sendInts : Int -> **!Int 1-> ()
sendInts 0 c = drop c
sendInts n c =
  sendInts (n - 1) (sendA n c)

main : ()
main =
  let (rx, wx) = newA @Int () in
  fork (\_ -> sendInts 10 wx) ;
  fork (\_ -> sendInts 10 wx) ;  -- Error: wx not in scope (already consumed)
  ()