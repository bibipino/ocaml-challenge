let fTF lst =
  match lst with
  | x::_::y::_::z::_ -> Some (x,y,z)
  | _ -> None
;;

assert(fTF ["cat"; "dog"] = None);;
assert(fTF [1; 2; 3; 4; 5; 6] = Some (1, 3, 5));;