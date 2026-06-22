module CloneReceiver where

main : ()
main =
  let (rx, wx) = newA @Int () in
  drop wx ;
  let (rx1, rx2) = cloneAS rx in  -- Error: cloneAS not defined for **?Int
  ()