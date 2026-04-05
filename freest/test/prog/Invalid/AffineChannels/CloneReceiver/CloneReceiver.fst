fstmodule CloneReceiver

-- Error: clone is not defined for **?T
main : ()
main =
  let (s, r) = newA in
  drop s;
  let (r1, r2) = clone r in
  ()
