let fail x = failwith("this fails");;
let max (a : int) (b : int) : int = 
  if a<0 || b<0 then 
    if a<0 then fail a else fail b
  else 
    if a>b then a else b;;

assert(max 2 5 = 5);;
assert(max 5 2 = 5);;
assert(try max (-2) 5 |> fun _ -> false with _ -> true);;
assert(try max 2 (-5) |> fun _ -> false with _ -> true);;
assert(try max (-2) (-5) |> fun _ -> false with _ -> true);;