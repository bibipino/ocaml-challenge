type nat = Z | S of nat;;

let rec iseven = function
   Z -> true
| S(S(n)) -> iseven n
| _ -> false
;;

assert(iseven(S(S(S(S(Z)))))= true);;

let rec halve = function
 Z -> Z
 | S(Z) -> Z
 | S(S(x)) -> let n = halve x in S(n)
;;

let nl = [Z;S(Z);S(S(Z));S(S(S(Z)));S(S(S(S(Z))));S(S(S(S(S(Z)))));S(S(S(S(S(S(Z))))));S(S(S(S(S(S(S(Z)))))))] in
assert(List.map halve nl = [Z; Z; S Z; S Z; S (S Z); S (S Z); S (S (S Z)); S (S (S Z))]);;


let rec add a b = match b with
  Z -> a
  | S(x) -> S(add a x)
;;

assert(add (S(S(Z))) (S(S(Z))) = S(S(S(S(Z)))));;

let rec mul a b = match b with
    Z -> Z
  | S(x) -> add a (mul a x)
;;

assert(mul (S(S(Z))) (S(S(S(S(Z))))) = S(S(S(S(S(S(S(S(Z)))))))));;

let rec equals a b = match (a,b) with
 (Z,Z) -> true
 | (Z, _) -> false
 | (_ , Z) -> false
 | (S(x), S(y)) -> equals x y
;;

assert(equals (S(S(Z))) (S(S(Z))) = true);;
assert(equals (S(Z)) (S(S(Z))) = false);;
assert(equals (S(S(Z))) (S(Z)) = false);;


let rec leq a b = match  (a,b) with
  (Z, _) -> true
 | (_ , Z) -> false
 | (S(x), S(y)) -> leq x y
;;

assert(leq (S(S(Z))) (S(S(Z))) = true);;
assert(leq (S(Z)) (S(S(Z))) = true);;
assert(leq (S(S(Z))) (S(Z)) = false);;