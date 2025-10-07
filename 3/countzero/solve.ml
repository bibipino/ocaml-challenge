let countzero f a b = 
  if a > b then failwith("wow") else
  let rec assegna f a b n = 
    if a = b then n + if f b = 0 then 1 else 0 else
      assegna f a (b-1) (n + if f b = 0 then 1 else 0)
  in
    assegna f a b 0
;;

assert (countzero (fun x -> x) (-10) 10 = 1);;

assert (countzero (fun x -> x) 1 10 = 0);;

assert (countzero (fun x -> x*x - 1) (-10) 10 = 2);;

assert (countzero (fun x -> (if x<0 then -x else x) - 1) (-10) 10 = 2);;