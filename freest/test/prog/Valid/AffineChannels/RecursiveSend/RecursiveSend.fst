module RecursiveSend where

sendDown : Int -> **!Int 1-> ()
sendDown 0 s = drop s
sendDown n s =
  sendDown (n - 1) (sendA n s)

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
  sendDown 5 s;
  collectAll r
