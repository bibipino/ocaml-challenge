let extract i l =
  let rec aux i l1 l2 =
    match l2 with
    | [] -> failwith "wut"
    | t :: h -> if i = 0 then 
      (t,l1 @ h) 
    else
      aux (i-1) (l1 @ [t]) h
  in
  if i >= List.length l then failwith "index out of bounds" else
  aux i [] l
;;

assert(extract 0 [1;2;3]= (1,[2;3])) ;;
assert(extract 1 [1;2;3]= (2,[1;3]));;
assert(extract 2 [1;2;3]= (3,[1;2]));;
assert(try extract 3 [1;2;3] |> fun _ -> false with _ -> true);;