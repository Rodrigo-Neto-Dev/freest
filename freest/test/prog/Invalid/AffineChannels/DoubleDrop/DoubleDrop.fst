module DoubleDrop where

main : ()
main =
  let (rx, wx) = new @**!Int () in
  drop wx ;
  drop wx   -- Error: wx not in scope
