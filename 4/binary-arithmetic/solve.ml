type bitint = ZT | UT | Z of bitint | U of bitint;;

let rec str_of_bitint = function
  | UT -> "1"
  | ZT -> "0"
  | Z (x) -> "0" ^ str_of_bitint x 
  | U (x) -> "1" ^ str_of_bitint x
;;

let int_of_bitint = function x -> str_of_bitint x;;