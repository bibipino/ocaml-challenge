let rec rev lst = 
  match lst with
  | [] -> []
  | h :: t -> (rev t) @ [h] 
;;

rev [5;4;3;2;1;0];;