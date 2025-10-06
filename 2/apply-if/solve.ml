let apply_if p f x = 
  if p x then 
    f x
else
  x
;;

let is_even = fun x -> x mod 2 = 0;;
let is_odd = fun x -> not (is_even x);;
let double x = x * 2;;

assert(apply_if is_even double 4 = 8);;
assert(apply_if is_even double 5 = 5);;
assert(apply_if is_odd double 4 = 4);;
assert(apply_if is_odd double 5 = 10);;