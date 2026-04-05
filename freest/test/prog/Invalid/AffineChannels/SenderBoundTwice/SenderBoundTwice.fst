fstmodule SenderBoundTwice

-- Error: s bound in pattern and used as if two independent senders exist
main : ()
main =
  let (s, r) = newA in
  let t = s in
  drop s;
  drop t
