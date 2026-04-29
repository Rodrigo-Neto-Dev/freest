module RecursiveSend where

sendDown : Int -> **!Int -> ()
sendDown 0 s = drop s
sendDown n s =
  sendA n s;
  sendDown (n - 1) s

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
  sendDown 5 s;
  collectAll r
