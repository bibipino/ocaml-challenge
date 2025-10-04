let nand1 (a : bool) (b : bool) : bool = not (a && b);;

assert(nand1 false false = true);;
assert(nand1 true true = false);;
assert(nand1 true false = true);;
assert(nand1 false true = true);;

let nand2 (a : bool) (b : bool) : bool = if a then
    if b then 
        false 
    else 
        true
else true;;
    

assert(nand2 false false = true);;
assert(nand2 true true = false);;
assert(nand2 true false = true);;
assert(nand2 false true = true);;

let nand3 (a : bool) (b : bool) : bool = match (a,b) with
| (true, true) -> false
| (_, _) -> true;;

assert(nand3 false false = true);;
assert(nand3 true true = false);;
assert(nand3 true false = true);;
assert(nand3 false true = true);;