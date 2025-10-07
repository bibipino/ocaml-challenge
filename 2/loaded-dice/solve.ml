let dice x = if x > 100 then failwith("simpaticone") else
  let y = Random.int(100)+1 in
  if y<=x then 6 else
    Random.int(5)+1
;;

assert(dice 0 <> 6);;
assert(dice 100 = 6);;