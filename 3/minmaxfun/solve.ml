let minmaxfun f a b = 
  if a > b then None else
  let rec minmax f a b mimi mama =
    if a = b then Some (min mimi (f b), max mama (f b)) else
      minmax f a (b-1) (min mimi (f b)) (max mama (f b))
  in 
  minmax f a b (f b) (f b)
;;

assert (minmaxfun (fun x -> x) (-2) 5 = Some (-2,5));;
assert (minmaxfun (fun x -> x) 5 (-2) = None);;
assert (minmaxfun (fun x -> x) 5 5 = Some (5,5));;
assert (minmaxfun (fun x -> x * x) (-2) 5 = Some (0,25));;

let curve x = x |> Float.of_int |> fun x -> x ** 3.0 -. 3.0 *. x;;
let arccos x = x |> Float.of_int |> Float.acos;;

assert (minmaxfun curve (-2) 2 = Some (-2.0,2.0));;
assert (minmaxfun arccos (-1) 1 = Some (0., Float.pi));;