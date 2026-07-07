module SendInts where

-- Three-message protocol.
type Send3 : 1S
type Send3 = !Int ; !Int ; !Int ; Close

sendInts : **!Send3 -1-> ()
sendInts c =
  let c1 = sendA 1 c in
  let c2 = sendA 2 c1 in
  let c3 = sendA 3 c2 in
  drop c3

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
  let (rx, wx) = newA @Send3 () in
  sendInts wx;
  print (sumInts rx)
