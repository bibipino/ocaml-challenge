let bounce n = 
  fun x -> if x mod (2*n) < n then x mod n else n - (x mod n);;

assert(bounce 3 10 = 2);;
assert(bounce 4 10 = 2);;
assert(bounce 5 10 = 0);;
assert(bounce 6 10 = 2);;
assert(bounce 7 10 = 4);;