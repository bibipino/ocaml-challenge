let (=?) f1 f2 = let inutile = f1 true || f2 true in
  (f1 true = f2 true) && (f1 false = f2 false) 
;;

assert ((fun b -> true) =? (fun b -> true));;
assert ((fun b -> b) =? (fun b -> true) = false);;
assert ((fun b -> false) =? (fun b -> true) = false);;
assert (not =? (fun b -> not (not b)) = false);;
assert ((fun b -> b) =? (fun b -> not (not b)));;