module ImplicitDuplication where

sendInts : Int -> **!Int -> ()
sendInts 0 c = drop c
sendInts n c =
  sendA n c ;
  sendInts (n - 1) c

main : ()
main =
  let (rx, wx) = new @**!Int () in
  fork (\_ -> sendInts 10 wx) ;
  fork (\_ -> sendInts 10 wx) ;  -- Error: wx not in scope (already consumed)
  ()