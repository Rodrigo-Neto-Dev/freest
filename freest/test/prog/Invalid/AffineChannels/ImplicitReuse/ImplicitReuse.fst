fstmodule ImplicitReuse

-- Error: recursive reuse of s without clone
loop : **!Int -> ()
loop s =
  sendA 0 s;
  loop s

main : ()
main =
  let (s, r) = newA in
  loop s
