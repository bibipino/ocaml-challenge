let xor1 (a : bool) (b : bool) : bool = (a || b) && not (a && b);;

assert(xor1 false false = false);;
assert(xor1 true true = false);;
assert(xor1 true false = true);;
assert(xor1 false true = true);;

let xor2 (a : bool) (b : bool) : bool = if a then
    if b then 
        false 
    else 
        true
else
    if b then
        true
    else
        false;;
    

assert(xor2 false false = false);;
assert(xor2 true true = false);;
assert(xor2 true false = true);;
assert(xor2 false true = true);;

let xor3 (a : bool) (b : bool) : bool = match (a,b) with
| (false, true) -> true
| (true, false) -> true
| (_, _) -> false;;

assert(xor3 false false = false);;
assert(xor3 true true = false);;
assert(xor3 true false = true);;
assert(xor3 false true = true);;