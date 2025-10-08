type nat = Z | S of nat;;

let rec iseven = function
   Z -> true
| S(S(n)) -> iseven n
| _ -> false
;;

let rec halve = function
 Z -> Z
 | S(Z) -> Z
 | S(S(x)) -> let n = halve x in S(n)
;;

let rec add a b = match b with
  Z -> a
  | S(x) -> S(add a x)
;;

let rec mul a b = match b with
    Z -> Z
  | S(x) -> add a (mul a x)
;;

let rec equals a b = match (a,b) with
 (Z,Z) -> true
 | (Z, _) -> false
 | (_ , Z) -> false
 | (S(x), S(y)) -> equals x y
;;

let rec leq a b = match  (a,b) with
  (Z, _) -> true
 | (_ , Z) -> false
 | (S(x), S(y)) -> equals x y