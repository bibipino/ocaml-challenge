let mux2 (s0 : bool) (a : bool) (b : bool) : bool = 
    (b && not s0) || (a && s0);;

let mux21 (s0 : bool) (a : bool) (b : bool) : bool = if s0 then
    a
else
    b;;
    
let mux22 (s0 : bool) (a : bool) (b : bool) : bool = match s0 with
| true -> a
| false -> b

let mux4 (s0 : bool) (s1 : bool) (a : bool) (b : bool) (c : bool) (d : bool) : bool = match (s0,s1) with
| (false, false) -> a
| (false, true) -> b
| (true, false) -> c
| (true, true) -> d;;

assert(mux4 false false false true false true = false);;
assert(mux4 false true false true false true = true);;
assert(mux4 true false false true false true = false);;
assert(mux4 true true false true false true = true);;


