let sum3 (a : int) (b : int) (c : int) : int = 
  a+b+c;;

let mr (a : int) (b : int) (c : int) : string = 
  let x = sum3 a b c in
  if x == 15 then "Masterpiece" else
    if x == 14 then "Highly recommended" else
      if x >= 11 then "Recommended" else "Mixed";;

assert(mr 1 1 1 = "Mixed");;
assert(mr 4 4 3 = "Recommended");;
assert(mr 4 4 4 = "Recommended");;
assert(mr 5 5 4 = "Highly recommended");;
assert(mr 5 5 5 = "Masterpiece");;