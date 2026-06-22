module DropOnlyTermination where

main : ()
main =
  let (rx, wx) = newA @Int () in
  drop wx ;
  case receiveA rx of
    Nothing   -> print "closed"
    Just (_, _) -> ()