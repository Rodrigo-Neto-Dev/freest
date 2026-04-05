fstmodule MultiProducerClone

collectAll : **?Int -> ()
collectAll r =
  case receiveA r of
    Nothing -> ()
    Just (x, r') ->
      print x;
      collectAll r'

main : ()
main =
  let (s, r) = newA in
  let (s1, s2) = clone s in
  sendA 10 s1;
  drop s1;
  sendA 20 s2;
  drop s2;
  collectAll r
