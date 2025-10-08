let apply lst k = 
  let rec search lst k =
    match lst with
    | [] -> None
    | x :: lst -> match x with
      | (x,y) -> if x = k then Some y else search lst k
  in 
    search lst k
;;

let f0 = [(1, 7); (2, 3); (4, 5); (5, 6); (7, 9); (2, 4); (8, 3)];;
assert(apply f0 4 = Some 5);;
assert(apply f0 6 = None);;
assert(apply f0 2 = Some 3);;

let mkfun lst =
  let rec dup lst nlist =
    match lst with
    | [] -> nlist
    | (k,v) :: lst -> if (apply nlist k) = None then
      dup lst (nlist @ [(k,v)]) else dup lst nlist
  in
    dup lst []
;;

assert(mkfun [(1,7);(2,3)] = [(1,7);(2,3)]);;
assert(mkfun [(1,7);(1,3)] = [(1,7)]);;
assert(mkfun [(1,7);(2,3);(1,5)] = [(1,7);(2,3)]);;
assert(mkfun [(1,7);(2,3);(1,5);(1,8)] = [(1,7);(2,3)]);;
assert(mkfun [(1,7);(2,3);(1,5);(1,8);(2,4)] = [(1,7);(2,3)]);;