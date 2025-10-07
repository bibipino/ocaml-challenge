let dice x = if x > 100 then failwith("simpaticone") else
  let y = Random.int(100)+1 in
  if y<=x then 6 else
    Random.int(5)+1
;;

let test p tot =
  let rec tr n p tot w =
    if n = 0 then string_of_float ( 100. *. (float_of_int w /. float_of_int tot)) ^ "%" else
      tr (n-1) p tot (w+ if dice p = 6 then 1 else 0)
  in 
  tr tot p tot 0;;

test 80 1000000;;

assert(dice 0 <> 6);;
assert(dice 100 = 6);;