let f1 x = x==4;;

let f2 x = if x then 0 else 1;;

let f3 x = if x=3 then (x,true) else (x,false);;

let f4 (x,y) = if y then x+1 else x;;

let f5 x y = x+y;;

let f6 x y = if x+y = 2 then true else false;;

let f7 x y = x && y+1==2;;

let f8 x y = if x || y then 1 else 0;;

let f9 x y = if x then y+1 else y;;

let f10 x = (x 1)+1;;

let f11 x = if (x 2) then 1 else 0;;

let f12 x = (x true) + 1;;

let f13 x = not (x 1);;

let f14 x = if (x true) then 1 else 0;;

let f15 x (y,z) = x+y+z;;

let f16 x y z = x+y+z;;

let f17 x y = x (x y+1) + 1;;

let f18 x = x (fun x -> x + 1) + 1;;

let f19 x = if (x 1)+1 = 2 then (fun x -> x && x) else (fun x -> x || x);;

let f20 x = if (x 1) then (fun true -> 2) else (fun true -> 2);;