fstmodule DropOnlyTermination

-- No messages sent; channel closed immediately
main : ()
main =
  let (s, r) = newA in
  drop s;
  case receiveA r of
    Nothing -> print "closed"
    Just (_, _) -> ()
