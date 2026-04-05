fstmodule RecursiveCloneProducers

-- Recursively clone and send; each level sends one value
fanSend : Int -> Int -> **!Int -> ()
fanSend 0 _ s = drop s
fanSend n v s =
  let (s1, s2) = clone s in
  sendA v s1;
  drop s1;
  fanSend (n - 1) (v + 1) s2

collectAll : **?Int -> ()
collectAll r =
  case receiveA r of
    Nothing -> ()
    Just (x, r') -> print x; collectAll r'

main : ()
main =
  let (s, r) = newA in
  fanSend 4 1 s;
  collectAll r
