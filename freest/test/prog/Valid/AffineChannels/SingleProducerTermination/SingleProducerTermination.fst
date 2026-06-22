module SingleProducerTermination where

main : ()
main =
  let (s, r) = newA in
  let s1 = sendA 1 s in
  let s2 = sendA 2 s1 in
  drop (sendA 3 s2);
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
