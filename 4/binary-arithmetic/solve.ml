type bitint = ZT | UT | Z of bitint | U of bitint;;

let rec str_of_bitint = function
  | UT -> "1"
  | ZT -> "0"
  | Z (x) -> "0" ^ str_of_bitint x 
  | U (x) -> "1" ^ str_of_bitint x
;;

let rec len = function
| ZT | UT -> 1
| Z x | U x -> 1+len x
;;

let rec pow = function
| ZT | UT -> 1
| Z x | U x -> 2*pow x
;;

let rec int_of_bitint = function
| ZT -> 0
| UT -> 1
| Z x-> int_of_bitint x
| U x -> 2 * pow x + int_of_bitint x
;;

assert(int_of_bitint (U(U(U(Z(UT)))))=29);;

let rec succ = function
| ZT -> UT
| UT -> U ZT
| Z x -> if len (succ x) = len x then Z (succ x) else succ x
| U x -> if len (succ x) = len x then U (succ x) else 
    match succ x with
    | U y -> U (Z y)
    | _ -> failwith "wtf"
;;

assert(int_of_bitint (succ (U(U(U(Z(UT))))))=30);;

let add bit1 bit2 =
  let rec addvero bit = function
  | 0 -> bit
  | x -> addvero (succ bit) (x-1)
  in
  addvero bit1 (int_of_bitint bit2)
;;

assert(int_of_bitint (add (U UT) (U(Z ZT)))= 7);;