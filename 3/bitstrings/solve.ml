type bitstring = E | Z of bitstring | U of bitstring;;

let rec string_of_bitstring = function
 E -> ""
 | Z(x) -> "0" ^ string_of_bitstring x
 | U(x) -> "1" ^ string_of_bitstring x
;;

assert(string_of_bitstring (Z(U(E))) = "01");;

let rec len = function
 E -> 0
 | Z(x) | U(x) -> 1 + len x
;;

assert(len (Z(E)) = 1);;
assert(len (E) = 0);;
assert(len (U(Z(U(Z(E))))) = 4);;

let rec countZ = function
 E -> 0
 | U(x) -> 0 + countZ x
 | Z(x) -> 1 + countZ x
;;

assert(countZ (Z(E)) = 1);;
assert(countZ (E) = 0);;
assert(countZ (U(Z(U(Z(E))))) = 2);;

let rec countU = function
 E -> 0
 | U(x) -> 1 + countU x
 | Z(x) -> 0 + countU x
;;

assert(countU (Z(E)) = 0);;
assert(countU (E) = 0);;
assert(countU (U(Z(U(Z(E))))) = 2);;

let rec concat b1 b2 = 
  match b2 with
  E -> b1
  | Z(x) -> Z(concat b1 x)
  | U(x) -> U(concat b1 x)
;;

assert(let s = Z(Z(Z(U(Z(U (Z E)))))) in len (concat s s) = 2 * len s);;

let rec equals b1 b2 =
  match b1,b2 with
  | E,E -> true
  | E,_ | _,E -> false
  | U(x), U(y) | Z(x), Z(y)-> equals x y
  | _ , _ -> false
;;

assert(let s = Z(Z(Z(U(Z(U (Z E)))))) in equals s s);;

let tl = function
    E -> E
  | U(x) | Z(x) -> x
;;

let rec prefix b1 b2 =
  match b1, b2 with
  | E, _ -> true
  | Z(x), Z(y) -> prefix x y
  | U(x), U(y) -> prefix x y
  | _ , _ -> false
;;

assert(let s = (Z(Z(Z(U(Z E))))) in prefix s (concat s ((Z(Z(Z(U(Z(U(Z E))))))))));;
assert(let s = (Z(Z(Z(U(Z(U (Z E))))))) in prefix s (concat s (concat s s)));;

let rec substring b1 b2 =
  if prefix b1 b2 then true else
    match tl b2 with
    | E -> false
    | s2 -> substring b1 s2
;;

assert(let s = (Z(U(Z E))) in substring s (concat s ((Z(Z(Z(U(Z(U(Z E))))))))));; (*il test aveva prefix al posto di substring quindi non andava*)