module InterleavingSumCheck where

-- Order nondeterministic; verify sum == 15
sumAll : **?Int 1-> Int -> Int
sumAll r acc =
  case receiveA r of
    Nothing -> acc
    Just (x, r') -> sumAll r' (acc + x)

producer : Int -> **!Int 1-> ()
producer x s =
  drop (sendA x s)

main : ()
main =
  let (s, r)   = newA in
  let (s1, s2) = cloneAS s in
  let (s2a, s2b) = cloneAS s2 in
  fork (producer 1 s1);
  fork (producer 5 s2a);
  fork (producer 9 s2b);
  let total = sumAll r 0 in
  print total
