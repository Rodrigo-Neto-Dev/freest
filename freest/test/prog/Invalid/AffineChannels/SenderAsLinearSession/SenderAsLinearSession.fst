module SenderAsLinearSession where

-- Error: 'send' expects a linear session channel ('!a; S'); an
-- affine sender wrapper ('**!(!Int; Close)') is not the same kind
-- of value.
type One : 1S
type One = !Int ; Close

main : ()
main =
  let (rx, wx) = newA @One () in
  send 1 wx   -- type error
