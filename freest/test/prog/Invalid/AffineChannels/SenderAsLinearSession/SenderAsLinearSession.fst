module SenderAsLinearSession where

-- Error: **!Int is not a session type; send expects !Int;S
main : ()
main =
  let (rx, wx) = new @**!Int () in
  send 1 wx   -- Error: type mismatch, send expects linear session channel