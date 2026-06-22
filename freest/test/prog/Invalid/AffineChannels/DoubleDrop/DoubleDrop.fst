module DoubleDrop where

main : ()
main =
  let (rx, wx) = newA @Int () in
  drop wx ;
  drop wx   -- Error: wx not in scope
