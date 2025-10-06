let is_even (n : int) : bool = n mod 2 = 0;;

let bOF (n : int) : bool = n>=1 && n<=5;;

let play (a : int) (b : int) : int = 
  if not (bOF a) && not (bOF b) 
    then 0
  else
    if not (bOF a) 
      then (-1)
    else
      if not (bOF b)
        then 1
      else
        if is_even (a+b) 
          then 1
        else
          (-1)
        ;;  

assert(play 1 2 = (-1));;
assert(play (-1) 2 = (-1));;
assert(play 1 6 = 1);;
assert(play (-1) (-1) = 0);;
assert(play 2 2 = 1);;