let double x = x * 2;;
let square x = x * x;;

let comp fun1 fun2 x =
  fun1 (fun2 x)
;;

assert((comp square double) 3 = 36);;
assert((comp double square) 3 = 18);;

assert(comp (fun b -> if b then 0 else 1) (fun x -> x>0) 3 = 0);;
assert(comp (fun b -> if b then 0 else 1) (fun x -> x>0) 0 = 1);;