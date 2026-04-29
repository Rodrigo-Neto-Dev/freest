module DropOnlyTermination where

main : ()
main =
  let (rx, wx) = new @**!Int () in
  drop wx ;
  case receiveA rx of
    Nothing   -> print "closed"
    Just (_, _) -> ()