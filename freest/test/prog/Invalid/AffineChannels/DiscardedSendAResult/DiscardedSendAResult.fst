module DiscardedSendAResult

main : ()
main =
  let (rx, wx) = newA @Int () in
  sendA 1 wx ;       -- Error: sendA's result (kind 1C) discarded by (;),
  drop wx             -- whose left operand requires kind *T
