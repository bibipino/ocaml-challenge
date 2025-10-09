let merge l1 l2 = 
  let rec aux nl l1 l2 =
    match l1,l2 with
    | [],[] -> nl
    | x,[] -> nl @ x
    | [],x -> nl @ x
    | t1::h1,t2::h2 -> if t1<=t2 then aux (nl @ [t1]) h1 (t2::h2) else aux (nl @ [t2]) (t1::h1) h2
  in 
  aux [] l1 l2
;;

assert (merge [1;4;5] [2;3;6] = [1;2;3;4;5;6]);;
assert (merge [7] [2;3;6] = [2;3;6;7]);;
assert (merge [7] [] = [7]);;


let halve = 
  let knife lst n =
  let rec sposta lst lst1 n =
    if n <= 0 then (lst,lst1) else
      match lst1 with
      | [] -> sposta lst [] (n-1)
      | h :: t -> sposta (lst @ [h]) t (n-1)
  in
    sposta [] lst n
  in
  function 
  | x -> knife x ((List.length x)/2)
;;

assert (halve [5;1;3;8;-2;6] = ([5;1;3], [8;-2;6]));;
assert (halve [1;3] = ([1], [3]));;
assert (halve [5;1;3] = ([5], [1;3]));;

let rec merge_sort l1 =
  match l1 with
  | [] -> []
  | [_] -> l1
  | _ -> let (l,r) = halve l1 in merge (merge_sort l) (merge_sort r)
;;

assert (merge_sort [5;1;3;8;-2;6] = [-2;1;3;5;6;8]);;