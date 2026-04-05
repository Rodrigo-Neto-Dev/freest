fstmodule ExactlyOneNothing

-- Counts Nothings; must be exactly 1
countNothing : **?Int -> Int -> Int
countNothing r acc =
  case receiveA r of
    Nothing -> acc + 1
    Just (_, r') -> countNothing r' acc

main : ()
main =
  let (s, r) = newA in
  sendA 42 s;
  drop s;
  print (countNothing r 0)
