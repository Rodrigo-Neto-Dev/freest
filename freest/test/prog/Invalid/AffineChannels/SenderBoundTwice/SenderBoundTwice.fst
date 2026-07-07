module SenderBoundTwice where

-- Error: 's' is bound twice (as 's' and 't') and then dropped via
-- both names. The affine channel only goes through one drop.
type One : 1S
type One = !Int ; Close

main : ()
main =
  let (s, r) = newA @One () in
  let t = s in
  case receiveA r of
    NothingL    -> ()
    JustL (_, _) -> ();
  drop s;
  drop t   -- error: s already consumed
