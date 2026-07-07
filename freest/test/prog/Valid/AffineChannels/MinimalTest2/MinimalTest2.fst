module MinimalTest2 where

-- Test 2: Does fork force VIO in the child thread?
main : ()
main =
  let (r, w) = newA @(!Int; Close) () in
  fork @() (\_ -1->
    let w1 = sendA 42 w in
    drop w1
  );
  case receiveA r of
    NothingL    -> print "FAIL: got Nothing"
    JustL (n, r1) ->
      let () = waitA r1 in
      print ("PASS: received " ++ show n)