module UseAfterDrop where

main : ()
main =
  let (rx, wx) = newA @Int () in
  drop wx ;
  sendA 1 wx   -- Error: wx not in scope