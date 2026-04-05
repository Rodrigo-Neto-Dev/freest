fstmodule ImplicitDuplication

consume : **!Int -> ()
consume s = sendA 0 s; drop s

-- Error: s passed to consume twice without clone
main : ()
main =
  let (s, r) = newA in
  consume s;
  consume s
