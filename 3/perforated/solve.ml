let rec perforated lst =
  match lst with
  | [] -> true
  | [x] -> true
  | x :: y :: rest -> (abs(x - y) > 1) && perforated (y :: rest)
;;

assert(perforated []);;
assert(perforated [1]);;
assert(perforated [1;2] = false);;
assert(perforated [1;3]);;
assert(perforated [1;5;2]);;
assert(perforated [1;3;2] = false);;
assert(perforated [1;4;2;0]);;
assert(perforated [1;3;2;0] = false);;
assert(perforated [1;3;5;2;4;7;3;1]);;