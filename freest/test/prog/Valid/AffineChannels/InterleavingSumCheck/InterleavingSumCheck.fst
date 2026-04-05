fstmodule InterleavingSumCheck

-- Order nondeterministic; verify sum == 15
sumAll : **?Int -> Int -> Int
sumAll r acc =
  case receiveA r of
    Nothing -> acc
    Just (x, r') -> sumAll r' (acc + x)

producer : Int -> **!Int -> ()
producer x s =
  sendA x s;
  drop s

main : ()
main =
  let (s, r)   = newA in
  let (s1, s2) = clone s in
  let (s2a, s2b) = clone s2 in
  fork (producer 1 s1);
  fork (producer 5 s2a);
  fork (producer 9 s2b);
  let total = sumAll r 0 in
  print total
