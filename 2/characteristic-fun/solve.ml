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

let f10 x = let y = fail x in if  then 1 else 0;;

let f11 x = let y = fail x in if  then 1 else 0;;

let f12 x = let y = fail x in if  then 1 else 0;;

let f13 x = let y = fail x in if  then 1 else 0;;

let f14 x = let y = fail x in if  then 1 else 0;;

let f15 x = let y = fail x in if  then 1 else 0;;

