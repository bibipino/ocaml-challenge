let exchange = function
| x when x > 100 -> failwith "nope"
| x when x < 10 -> x*10
| x -> (x/10) + (x mod 10)*10
;;

assert(exchange 10 = 1);;
assert(exchange 2 = 20);;
assert(try exchange 101 |> fun _ -> false with _ -> true);;

let is_valid_answer = function (a,b) -> (b * 4) = a && ((exchange a) * 3) = exchange b;;

let find (a , b : int * int) : int * int=
  if b>a then (-1,-1) else 
  let rec trueFind n max=
    if n*4 >= max then (-1,-1) else if is_valid_answer (n*4,n) then (n*4,n) else trueFind (n+1) max
  in
    trueFind b a
;;

assert(find (100,0) = (0,0));;
assert(find (100,10) = (72,18));;
assert(find (0,100) = (-1,-1));;