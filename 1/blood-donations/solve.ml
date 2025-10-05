type blood = A | B | AB | O;;

let check_groups (a : blood) (b : blood) : bool =
    match (a,b) with
    | (A, O) -> false
    | (B, O) -> false
    | (A, B) -> false
    | (B, A) -> false
    | (AB, AB) -> true
    | (AB, _) -> false
    | (_, _) -> true
;;

assert (check_groups O O = true);;
assert (check_groups O A = true);;
assert (check_groups O B = true);;
assert (check_groups O AB = true);;
assert (check_groups A O = false);;
assert (check_groups A A = true);;
assert (check_groups A B = false);;
assert (check_groups A AB = true);;
assert (check_groups B O = false);;
assert (check_groups B A = false);;
assert (check_groups B B = true);;
assert (check_groups B AB = true);;
assert (check_groups AB O = false);;
assert (check_groups AB A = false);;
assert (check_groups AB B = false);;
assert (check_groups AB AB = true);;