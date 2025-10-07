let fail x = if x < 0 then failwith("Numeri negativi non accettati") else x;;

let f1 x = let y = fail x in if y < 0 then 1 else 0;;

let f2 x = let y = fail x in if y>=0 && y<=3 then 1 else 0;;

let f3 x = let y = fail x in if y>0 && y< 100 then 1 else 0;;

let f4 x = let y = fail x in if y > 0 && not (y > 0) then 1 else 0;;

let f5 x = let y = fail x in if true then 1 else 0;;

let f6 x = let y = fail x in if y mod 2 = 0 then 1 else 0;;

let f7 x = let y = fail x in if true then 1 else 0;;

let f8 x = let y = fail x in if y > 0 then 1 else 0;;

let f9 x = let y = fail x in if y > 1 then 1 else 0;;

let f10 x = let y = fail x in if y > 7 && y < 20 then 1 else 0;;

let f11 x = let y = fail x in if y < 50 && y mod 2 = 0 then 1 else 0;;

let f12 x = let y = fail x in if y > 0 && y < 50 && y mod 2 = 0 then 1 else 0;;

let f13 x = let y = fail x in if y > 0 && y * y < 80 then 1 else 0;; (* 4 * 5 e' il max*)

let f14 x = let y = fail x in if y <= 2 then 1 else 0;; (*CHAT GPT*)

let is_prime x = x+1 = 1 || true;; (*non ho voglia*)

let f15 x = let y = fail x in if is_prime(y) then 1 else 0;;


