module MultiProducerClone where

collectAll : **?Int 1-> ()
collectAll r =
  case receiveA r of
    Nothing -> ()
    Just (x, r') ->
      print x;
      collectAll r'

main : ()
main =
  let (s, r) = newA in
  let (s1, s2) = cloneAS s in
  drop (sendA 10 s1);
  drop (sendA 20 s2);
  collectAll r
