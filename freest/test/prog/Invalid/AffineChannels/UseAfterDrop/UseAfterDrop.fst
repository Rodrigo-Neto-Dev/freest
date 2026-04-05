fstmodule UseAfterDrop

-- Error: s used after drop
main : ()
main =
  let (s, r) = newA in
  drop s;
  sendA 99 s
