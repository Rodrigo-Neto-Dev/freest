fstmodule LargeCloneFan

-- Fan out to 4 producers via nested clones; verify sum
producer : Int -> **!Int -> ()
producer x s = sendA x s; drop s

sumAll : **?Int -> Int -> Int
sumAll r acc =
  case receiveA r of
    Nothing -> acc
    Just (x, r') -> sumAll r' (acc + x)

main : ()
main =
  let (s, r)     = newA in
  let (s1, sR)   = clone s  in
  let (s2, sRR)  = clone sR in
  let (s3, s4)   = clone sRR in
  fork (producer 1 s1);
  fork (producer 2 s2);
  fork (producer 3 s3);
  fork (producer 4 s4);
  print (sumAll r 0)
