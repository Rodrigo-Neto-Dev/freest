module BranchMissingDrop where

sendInts : Int -> **!Int 1-> ()
sendInts 0 c = drop c
sendInts n c =
  sendInts (n - 1) (sendA n c)

main : ()
main =
  let (rx, wx) = newA @Int () in
  let b = True in
  if b
  then sendInts 3 wx
  else ()   -- Error: wx not consumed in else branch