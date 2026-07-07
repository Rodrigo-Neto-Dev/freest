module ImplicitReuse where

-- Error: 'c' is consumed by 'sendA', so the recursive call's reuse
-- of 'c' is out of scope.
type One : 1S
type One = !Int ; Close

loop : **!One -1-> ()
loop c =
  let _ = sendA 0 c in
  loop c   -- error
