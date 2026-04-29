module BranchMissingDrop where

sendInts : Int -> **!Int -> ()
sendInts 0 c = drop c
sendInts n c =
  sendA n c ;
  sendInts (n - 1) c

main : ()
main =
  let (rx, wx) = new @**!Int () in
  let b = True in
  if b
  then sendInts 3 wx
  else ()   -- Error: wx not consumed in else branch