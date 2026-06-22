module RecursiveCloneProducers where

-- Recursively cloneAS and send; each level sends one value
fanSend : Int -> Int -> **!Int 1-> ()
fanSend 0 _ s = drop s
fanSend n v s =
  let (s1, s2) = cloneAS s in
  drop (sendA v s1);
  fanSend (n - 1) (v + 1) s2

collectAll : **?Int 1-> ()
collectAll r =
  case receiveA r of
    Nothing -> ()
    Just (x, r') -> print x; collectAll r'

main : ()
main =
  let (s, r) = newA in
  fanSend 4 1 s;
  collectAll r
