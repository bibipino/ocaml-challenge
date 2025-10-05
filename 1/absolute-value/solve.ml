let abs (a : int) : int = 
  if a>0 then
    a
  else
    -a
;;

assert(abs 1 = 1);;
assert(abs 0 = 0);;
assert(abs (-1) = 1);;