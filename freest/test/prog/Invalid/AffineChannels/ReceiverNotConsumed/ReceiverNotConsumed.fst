module ReceiverNotConsumed

-- Error: rx goes out of scope without being consumed
main : ()
main =
  let (rx, wx) = new @**!Int () in
  drop wx
  -- rx not consumed: error