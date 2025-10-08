let rndLst n m = 
  let rec aux lst n m =
    if n = 0 then lst else
      aux (lst @ [(Random.int(m)+1)]) (n-1) m
  in
    aux [] n m
;;

let rec rndLst2 n m =
  if n = 0 then []
  else (Random.int(m)+1) :: rndLst2 (n-1) m
;;

let rando1 = rndLst 100 20;;
let rando2 = rndLst2 100 20;;


float_of_int (List.fold_right (+) rando1 0) /. (float_of_int (List.length rando1));;
float_of_int (List.fold_right (+) rando2 0) /. (float_of_int (List.length rando2));;
