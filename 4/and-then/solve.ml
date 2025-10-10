let ( -?-> ) (o : 'a option) (next : 'a -> 'b option) : 'b option =
  match o with
  | None -> None
  | Some x -> next x
;;

let fTF lst =
  match lst with
  | x::_::y::_::z::_ -> Some (x,y,z)
  | _ -> None
;;

(*Stessa cosa di sotto ma qui se c'e' None esso e' restituito in automatico*)
let nftf lst = 
  List.nth_opt lst 0 -?-> fun x ->
  List.nth_opt lst 2 -?-> fun y ->
  List.nth_opt lst 4 -?-> fun z ->
  Some(x,y,z)
;;

assert(nftf [1;2;3;4]= None);;
assert(nftf [1;2;3;4;5;6]=Some(1,3,5));;

(*Questo e' quello che accadrebbe senza -?-> operator*)
let nnftf lst = (
  fun x -> fun y -> fun z -> 
  match (x,y,z) with
  | (Some x, Some y, Some z) -> Some(x,y,z)
  | (_,_,_) -> None
) 
(List.nth_opt lst 0) (List.nth_opt lst 2) (List.nth_opt lst 4)
;;

assert(nnftf [1;2;3;4]= None);;
assert(nnftf [1;2;3;4;5;6]=Some(1,3,5));;