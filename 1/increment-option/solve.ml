let ret_op (a : int option) : int =
  match a with
    None -> 0
  | Some x -> x;;

let incr_opt (a : int option) : int option = 
  if ((ret_op(a) + 1 )= 0) then 
    None
  else
    Some ((ret_op a) + 1);;

assert(incr_opt (Some (-1)) = None);;
assert(incr_opt (Some 0) = Some 1);;
assert(incr_opt (Some 1) = Some 2);;
assert(incr_opt (Some (-2)) = Some (-1));;
