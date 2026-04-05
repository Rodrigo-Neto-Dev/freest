fstmodule ReceiverDuplication

consume : **?Int -> ()
consume r =
  case receiveA r of
    Nothing -> ()
    Just (_, r') -> consume r'

-- Error: r passed to consume twice
main : ()
main =
  let (s, r) = newA in
  drop s;
  consume r;
  consume r
