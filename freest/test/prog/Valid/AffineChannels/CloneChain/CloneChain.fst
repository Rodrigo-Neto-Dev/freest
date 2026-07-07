module CloneChain where

-- Fixed-depth protocol: three messages then close.
type Three : 1S
type Three = !Int ; !Int ; !Int ; Close

-- Each clone independently sends one Int and drops mid-stream.
sendOne : Int -> **!Three -1-> ()
sendOne x c = drop (sendA x c)

-- Receiver continuation 'c3 : **?(Wait)' is consumed explicitly via
-- 'waitA' rather than discarded via '_' (affine capabilities cannot be
-- silently discarded under the capability/session separation).
sumInts : **?(?Int ; ?Int ; ?Int ; Wait) -1-> Int
sumInts c =
  case receiveA c of
    NothingL    -> 0
    JustL (n1, c1) ->
      case receiveA c1 of
        NothingL    -> n1
        JustL (n2, c2) ->
          case receiveA c2 of
            NothingL    -> n1 + n2
            JustL (n3, c3) ->
              let _ = waitA c3 in
              n1 + n2 + n3

main : ()
main =
  -- Prelude newA signature: tuple is (receiver, sender).
  let (rx, wx) = newA @Three () in
  let (wx1, wxR) = cloneAS wx in
  let (wx2, wx3) = cloneAS wxR in
  -- Fork thunks must be linear ('-1->') because they capture the
  -- linear sender clones wx1/wx2/wx3 exactly once.
  fork @() (\_ -1-> sendOne 1 wx1);
  fork @() (\_ -1-> sendOne 2 wx2);
  fork @() (\_ -1-> sendOne 3 wx3);
  print (sumInts rx)
