module MinimalTest where

main : Int
main =
  let (r, w) = newA @Int () in
  let w1 = sendA 42 w in
  let w2 = drop w1 in
  case receiveA r of
    NothingL    -> 0
    JustL (n, _) -> n