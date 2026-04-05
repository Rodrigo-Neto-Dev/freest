fstmodule BranchMissingDrop

-- Error: s not dropped in the False branch
main : ()
main =
  let (s, r) = newA in
  let b = True in
  if b
  then drop s
  else sendA 1 s
