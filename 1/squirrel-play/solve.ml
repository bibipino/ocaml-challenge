type season = Spring | Summer | Autumn | Winter ;;

let squirrel_play (t : int) (s : season) : bool = 
  if t >= 15 && ((t<=30) || (s == Summer && t <= 35)) then true else false
;;

assert(squirrel_play 18 Winter = true);;
assert(squirrel_play 32 Spring = false);;
assert(squirrel_play 32 Summer = true);;
assert(squirrel_play 11 Autumn = false);;