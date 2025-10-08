let knife lst n =
  let rec sposta lst lst1 n =
    if n <= 0 then (lst,lst1) else
      match lst1 with
      | [] -> sposta lst [] (n-1)
      | h :: t -> sposta (lst @ [h]) t (n-1)
  in
    sposta [] lst n
;;

assert(knife [1;2;3;4;5;6] 3 = ([1;2;3], [4;5;6]));;
assert(knife ['b';'r';'e';'a';'d'] 3 = (['b';'r';'e'], ['a';'d']));;
assert(knife [] 0 = ([], []));;
assert(knife ["miss";"me"] 2 = (["miss";"me"], []));;
assert(knife ["ops"] (-1) = ([],["ops"]));;
