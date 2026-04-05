fstmodule DoubleDrop

-- Error: s dropped twice
main : ()
main =
  let (s, r) = newA in
  drop s;
  drop s
