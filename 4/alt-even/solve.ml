let alt_even =
  let rec int_to_list lst = 
    function
  | x when x = 0 -> lst
  | x -> int_to_list ([x mod 10] @ lst) (x/10)
in
  let rec next_even = function
  | [] -> true
  | t::h -> if t mod 2 = 0 then next_odd h else false
  and
  next_odd = function
  | [] -> true
  | t::h -> if t mod 2 = 1 then next_even h else false
in
  function
  | x -> next_even (List.rev(int_to_list [] x))
;;

assert(alt_even 1234);;
assert(not (alt_even 1234567));;