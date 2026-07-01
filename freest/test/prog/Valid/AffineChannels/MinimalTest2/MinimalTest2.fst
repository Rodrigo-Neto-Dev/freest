module MinimalTest2 where

-- Test 2: Does fork force VIO in the child thread?
main : ()
main =
  let (r, w) = newA @Int () in
  fork (\_ ->
    let w1 = sendA 42 w in
    drop w1
  );
  case receiveA r of
    NothingL    -> print "FAIL: got Nothing"
    JustL (n, _) -> print ("PASS: received " ++ show n)