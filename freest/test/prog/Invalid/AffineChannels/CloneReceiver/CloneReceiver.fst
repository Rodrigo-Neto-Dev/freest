module CloneReceiver where

main : ()
main =
  let (rx, wx) = new @**!Int () in
  drop wx ;
  let (rx1, rx2) = clone rx in  -- Error: clone not defined for **?Int
  ()