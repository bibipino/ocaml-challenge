let rndLst n m = 
  let rec aux lst n m =
    if n = 0 then lst else
      aux (lst @ [(Random.int(m)+1)]) (n-1) m
  in
    aux [] n m
;;

let randoo = rndLst 100 20;;

float_of_int (List.fold_right (+) randoo 0) /. (float_of_int (List.length randoo));;