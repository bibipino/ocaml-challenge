let (<*>) f1 x = match (f1, x) with
  (None, _) -> None
  | (_, None) -> None
  | (Some f, Some n) -> Some (f n)
;;

let square x = x * x
let double x = 2 * x
let multiply x y = x * y;;

assert (Some square <*> None = None);;
assert (None <*> Some 2 = None);;
assert (None <*> (Some double <*> Some 2) = None);;
assert (Some multiply <*> Some 3 <*> Some 2 = Some 6);;
assert (Some multiply <*> None <*> Some 2 = None);;