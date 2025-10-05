let parrot_trouble (a : bool) (h : int) : bool option = 
  if h > 23 || h < 0 then 
    None
  else
    if a then 
      if h < 7 || h > 20 then
        Some false
      else
        Some true
    else
      Some true
;;

assert(parrot_trouble true (-1) = None);;
assert(parrot_trouble true 0 = Some false);;
assert(parrot_trouble true 1 = Some false);;
assert(parrot_trouble true 2 = Some false);;
assert(parrot_trouble true 3 = Some false);;
assert(parrot_trouble true 4 = Some false);;
assert(parrot_trouble true 5 = Some false);;
assert(parrot_trouble true 6 = Some false);;
assert(parrot_trouble true 7 = Some true);;
assert(parrot_trouble true 8 = Some true);;
assert(parrot_trouble true 9 = Some true);;
assert(parrot_trouble true 10 = Some true);;
assert(parrot_trouble true 11 = Some true);;
assert(parrot_trouble true 12 = Some true);;
assert(parrot_trouble true 13 = Some true);;
assert(parrot_trouble true 14 = Some true);;
assert(parrot_trouble true 15 = Some true);;
assert(parrot_trouble true 16 = Some true);;
assert(parrot_trouble true 17 = Some true);;
assert(parrot_trouble true 18 = Some true);;
assert(parrot_trouble true 19 = Some true);;
assert(parrot_trouble true 20 = Some true);;
assert(parrot_trouble true 21 = Some false);;
assert(parrot_trouble true 22 = Some false);;
assert(parrot_trouble true 23 = Some false);;
assert(parrot_trouble true 24 = None);;
assert(parrot_trouble false (-1) = None);;
assert(parrot_trouble false 0 = Some true);;
assert(parrot_trouble false 1 = Some true);;
assert(parrot_trouble false 2 = Some true);;
assert(parrot_trouble false 3 = Some true);;
assert(parrot_trouble false 4 = Some true);;
assert(parrot_trouble false 5 = Some true);;
assert(parrot_trouble false 6 = Some true);;
assert(parrot_trouble false 7 = Some true);;
assert(parrot_trouble false 8 = Some true);;
assert(parrot_trouble false 9 = Some true);;
assert(parrot_trouble false 10 = Some true);;
assert(parrot_trouble false 11 = Some true);;
assert(parrot_trouble false 12 = Some true);;
assert(parrot_trouble false 13 = Some true);;
assert(parrot_trouble false 14 = Some true);;
assert(parrot_trouble false 15 = Some true);;
assert(parrot_trouble false 16 = Some true);;
assert(parrot_trouble false 17 = Some true);;
assert(parrot_trouble false 18 = Some true);;
assert(parrot_trouble false 19 = Some true);;
assert(parrot_trouble false 20 = Some true);;
assert(parrot_trouble false 21 = Some true);;
assert(parrot_trouble false 22 = Some true);;
assert(parrot_trouble false 23 = Some true);;
assert(parrot_trouble false 24 = None);;
