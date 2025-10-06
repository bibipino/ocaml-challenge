type acc = SR | WR | WA | SA;;

let acc (a : acc) (b : acc) (c : acc) : bool =
  if a == SR || b == SR || c == SR then 
    false
  else
    if (a == WR && (b == WR || c == WR)) || (b == WR && c == WR) then 
      false
  else 
    true
;;

assert(acc SR SR SR = false);;
assert(acc WR SR SR = false);;
assert(acc WA SR SR = false);;
assert(acc SA SR SR = false);;
assert(acc SR WR SR = false);;
assert(acc WR WR SR = false);;
assert(acc WA WR SR = false);;
assert(acc SA WR SR = false);;
assert(acc SR WA SR = false);;
assert(acc WR WA SR = false);;
assert(acc WA WA SR = false);;
assert(acc SA WA SR = false);;
assert(acc SR SA SR = false);;
assert(acc WR SA SR = false);;
assert(acc WA SA SR = false);;
assert(acc SA SA SR = false);;

assert(acc SR SR WR = false);;
assert(acc WR SR WR = false);;
assert(acc WA SR WR = false);;
assert(acc SA SR WR = false);;
assert(acc SR WR WR = false);;
assert(acc WR WR WR = false);;
assert(acc WA WR WR = false);;
assert(acc SA WR WR = false);;
assert(acc SR WA WR = false);;
assert(acc WR WA WR = false);;
assert(acc WA WA WR = true);;
assert(acc SA WA WR = true);;
assert(acc SR SA WR = false);;
assert(acc WR SA WR = false);;
assert(acc WA SA WR = true);;
assert(acc SA SA WR = true);;

assert(acc SR SR WA = false);;
assert(acc WR SR WA = false);;
assert(acc WA SR WA = false);;
assert(acc SA SR WA = false);;
assert(acc SR WR WA = false);;
assert(acc WR WR WA = false);;
assert(acc WA WR WA = true);;
assert(acc SA WR WA = true);;
assert(acc SR WA WA = false);;
assert(acc WR WA WA = true);;
assert(acc WA WA WA = true);;
assert(acc SA WA WA = true);;
assert(acc SR SA WA = false);;
assert(acc WR SA WA = true);;
assert(acc WA SA WA = true);;
assert(acc SA SA WA = true);;

assert(acc SR SR SA = false);;
assert(acc WR SR SA = false);;
assert(acc WA SR SA = false);;
assert(acc SA SR SA = false);;
assert(acc SR WR SA = false);;
assert(acc WR WR SA = false);;
assert(acc WA WR SA = true);;
assert(acc SA WR SA = true);;
assert(acc SR WA SA = false);;
assert(acc WR WA SA = true);;
assert(acc WA WA SA = true);;
assert(acc SA WA SA = true);;
assert(acc SR SA SA = false);;
assert(acc WR SA SA = true);;
assert(acc WA SA SA = true);;
assert(acc SA SA SA = true);;