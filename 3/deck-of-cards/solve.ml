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

let rec mem x set = 
    match set with
    | [] -> false
    | t :: h -> x = t || mem x h
;;

let rec subseteq l1 l2 =
  match l1 with
  [] -> true
  | t :: h -> (mem t l2) && subseteq h l2
;;

let rec dup = function
  [] -> false
  | t :: h -> (subseteq [t] h) || dup h
;;

(*imported functions to make my life easier*)


type suit = Spades | Hearts | Diamonds | Clubs;;
type card = Card of int * suit;;

let is_complete dc =
  if List.length dc <> 40 then false else not (dup dc)
;;

let rnd_dc () =
  let rec gen (s : suit) (lst : int list) =
    if List.length lst = 10 then List.map (fun x -> (x,s)) lst else
      let rnd = Random.int(10)+1 in
      if (mem rnd lst) then gen s lst else gen s (lst@[rnd])
  in
    let semi = gen Spades [] @ gen Hearts [] @ gen Diamonds [] @ gen Clubs [] 
  in
    let rec shuffle = function
      [] -> []
      | x -> let rnd = Random.int(List.length x)
        in let (x,l) = extract rnd x
        in x :: shuffle l 
  in
    shuffle semi
;;

assert(is_complete(rnd_dc ())=true);;
assert(is_complete(rnd_dc ())=true);;
assert(is_complete(rnd_dc ())=true);;
assert(is_complete(rnd_dc ())=true);;