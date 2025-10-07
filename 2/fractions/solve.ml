let is_pos_frac ((n : int) , (d : int)) : bool =
  if (n < 0 && not (d < 0)) || (d < 0 && not (n < 0)) then false else true
;;

let cpf ((a : int),(b : int)) ((c : int),(d : int)) : int =
  if not (is_pos_frac((a,b))) || not (is_pos_frac((c,d))) then failwith("negativa") else 
    let x = a * d in let y = b * c in
    if x = y then 0 else 
      if x > y then 1 else (-1)
;;

assert(cpf (1,2) (2,4) = 0);;
assert(cpf (1,2) (1,3) = 1);;
assert(try cpf ((-1),2) (2,4) |> fun _ -> false with _ -> true);;
assert(cpf (1,2) (2,3) = -1);;
assert(cpf (1,2) ((-2),(-4)) = 0);;
