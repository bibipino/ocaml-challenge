let max (a : int option) (b : int option) : int option = 
  if a == None || b == None then 
    if a == None && b == None 
      then None 
    else
      if a == None 
        then b 
      else 
        a
  else 
    if a > b
      then a
    else
      b
;;

let best (a : int option) (b : int option) (c : int option) : int option =
  max a (max b c)  
;;

assert(best (Some 100) (Some 200) (Some 150) = Some 200);;
assert(best (Some 100) None (Some 150) = Some 150);;
assert(best None None None = None);;
assert(best None (Some 300) None = Some 300);;