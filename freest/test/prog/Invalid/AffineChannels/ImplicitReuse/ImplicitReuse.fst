module RecursiveReuseWithoutClone where

-- Error: wx implicitly reused across recursive call
loop : **!Int 1-> ()
loop c =
  sendA 0 c ;
  loop c   -- Error: c not in scope (consumed by first sendA iteration)