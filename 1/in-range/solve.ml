let in_range (n : int) (min : int) (max : int) : bool =
  if max < min then failwith("Incorrect sintax") else
    n>=min && n<=max;;

assert(in_range 1 0 2 = true);;
assert(try in_range 1 2 1 |> fun _ -> false with _ -> true);;
assert(in_range 1 2 3 = false);;
assert(in_range 4 0 2 = false);;