let option_map (fun1 : 'a -> 'b) (n : 'a option) : 'b option = 
  match n with 
  None -> None
  | Some x -> Some (fun1 x)
;;
let double x = x * 2;;
let square x = x * x;;

assert (option_map double (Some 3) = Some 6);;
assert (option_map double None = None);;
assert (option_map double (Some (square 3)) = Some 18);;
assert (option_map square (option_map double None) = None);;