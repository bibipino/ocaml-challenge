let string_of_list lst = 
  let rec aux = function
  | [] -> ""
  | [x] -> string_of_int x
  | h :: t -> string_of_int h ^ ";" ^ aux t
  in
    "[" ^ (aux lst) ^ "]"
;;

assert(string_of_list [] = "[]");;
assert(string_of_list [1;2;3] = "[1;2;3]");;
assert(string_of_list [1] = "[1]");;
assert(string_of_list [31527;2] = "[31527;2]");;