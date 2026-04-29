module SingleProducerTermination where

main : ()
main =
  let (s, r) = newA in
  sendA 1 s;
  sendA 2 s;
  sendA 3 s;
  drop s;
  case receiveA r of
    Nothing -> ()
    Just (x, r1) ->
      print x;
      case receiveA r1 of
        Nothing -> ()
        Just (y, r2) ->
          print y;
          case receiveA r2 of
            Nothing -> ()
            Just (z, r3) ->
              print z;
              case receiveA r3 of
                Nothing -> print "done"
                Just (_, _) -> ()
