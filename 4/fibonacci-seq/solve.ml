let rec fib = function
| x when x<0 -> failwith "negative"
| 0 -> []
| 1 -> [0]
| 2 -> [0;1]
| n -> match (List.rev (fib(n-1))) with
  | h1::h2::t -> fib(n-1) @ [h1+h2]
;;

assert(fib 10 = [0; 1; 1; 2; 3; 5; 8; 13; 21; 34]);;
assert(fib 7 = [0; 1; 1; 2; 3; 5; 8]);;