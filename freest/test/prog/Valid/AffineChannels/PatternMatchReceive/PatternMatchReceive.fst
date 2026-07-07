module PatternMatchReceive where

-- Three-message protocol. Writer sends 10, 20, 30 then drops.
type Three : 1S
type Three = !Int ; !Int ; !Int ; Close

sumInts : **?(?Int ; ?Int ; ?Int ; Wait) -1-> Int
sumInts c =
  case receiveA c of
    NothingL -> 0
    JustL (n1, c1) ->
      case receiveA c1 of
        NothingL -> n1
        JustL (n2, c2) ->
          case receiveA c2 of
            NothingL -> n1 + n2
            JustL (n3, c3) ->
              waitA c3;
              n1 + n2 + n3

main : ()
main =
  let (rx, wx) = newA @Three () in
  let wx1 = sendA 10 wx in
  let wx2 = sendA 20 wx1 in
  let wx3 = sendA 30 wx2 in
  drop wx3;
  print (sumInts rx)
