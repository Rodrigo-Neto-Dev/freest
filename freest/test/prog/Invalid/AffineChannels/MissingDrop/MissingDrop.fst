fstmodule MissingDrop

-- Error: s escapes scope without drop or sendA consuming it
main : ()
main =
  let (s, r) = newA in
  sendA 1 s
