type grade = Val of int | Lode ;;

let isVal (grade : grade) : bool =
  match grade with
  | Lode -> true
  | Val x -> x>=18 && x<=30

let int_of_grade grade =
  if not (isVal grade) then failwith("wawa") else
    match grade with
    | Lode -> 32
    | Val x -> x;;

let avg_norec lst = float_of_int(List.fold_right (+) (List.map int_of_grade lst) 0) /. (float_of_int (List.length lst));;

assert(avg_norec [(Val 18);Lode;(Val 22)] = 24.);;