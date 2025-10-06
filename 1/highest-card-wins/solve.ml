type card = Joker | Val of int;;

let valid = function
  Joker -> true
  | Val x -> x>=1 && x<=10;; 
let win (pl : card) (deal : card) : bool =
  if valid pl && valid deal then 
    if (deal == Joker) then false else 
      if (pl == Joker) then true else
        match (pl,deal) with
        | (Val x, Val y) -> x>y
        | (_ , _) -> failwith("Non dovrebbe essere possibile")
  else failwith("Che scocciatura che siete")
;;

assert(win Joker (Val 7) = true);;
assert(win (Val 7) Joker = false);;
assert(win Joker Joker = false);;
assert(win (Val 9) (Val 7) = true);;
assert(win (Val 7) (Val 7) = false);;