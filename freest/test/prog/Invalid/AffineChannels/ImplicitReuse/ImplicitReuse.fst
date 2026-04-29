module RecursiveReuseWithoutClone where

-- Error: wx implicitly reused across recursive call
loop : **!Int -> ()
loop c =
  sendA 0 c ;
  loop c   -- Error: c not in scope (consumed by first sendA iteration)