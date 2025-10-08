let consecutive_even lst =
  let rec aux lst champ current =
    match lst with
    | [] -> champ
    | h :: t -> if h mod 2 = 0 then 
      aux t (max (current+1) champ) (current+1) else
        aux t champ 0
    in 
      aux lst 0 0
;;

assert(consecutive_even [] = 0);;
assert(consecutive_even [1;2;3;4;5;6] = 1);; 
assert(consecutive_even [1;2;2;3;4;5] = 2);;
assert(consecutive_even [1;2;3;4;2;5] = 2);;
assert(consecutive_even [1;2;2;3;4;2;5] = 2);;
assert(consecutive_even [1;2;2;2;3;4;2;6;5] = 3);;