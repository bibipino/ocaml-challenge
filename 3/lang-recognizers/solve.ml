let rec count x l = List.fold_left (fun c y -> if y=x then c+1 else c) 0 l;;
let rec lang0 = function
  | [] -> true
  | [x] -> not (x = 0)
  | t :: h -> lang0 h
;;

assert(lang0 [1;1;1;1;1;1;1;1;1;1;1;1]= true);;
assert(lang0 [1;1;1;1;1;1;1;1;1;1;1;0]= false);;
assert(lang0 [1;1;1;1;1;1;1;1;1;1;0;0]= false);;
assert(lang0 [1;1;1;1;1;0;1;0;1;0;1;1]= true);;
assert(lang0 [1;0;0;1;1;1;1;1;1;1;0;1]= true);;

let rec lang1 = function
| [] -> true
| 0::l -> not (List.mem 0 l) && lang1 l
| x::l -> lang1 l
;;

assert(lang0 [1;1;1;1;1;1;1;1;1;1;1;1]= true);;
assert(lang0 [1;1;1;1;1;1;1;1;1;1;1;0]= false);;
assert(lang0 [1;1;1;1;1;1;1;1;1;1;0;0]= false);;
assert(lang0 [1;1;1;1;1;0;1;0;1;0;1;1]= true);;
assert(lang0 [1;0;0;1;1;1;1;1;1;1;0;1]= true);;

let rec lang2 = function
| [] -> true
| 1::l -> not (List.mem 0 l) && lang2 l
| x::l -> lang2 l
;;
assert(lang2 [0;2;0;2;2;0;1;2;2;1;2;1]=true);;
assert(lang2 [0;2;0;2;2;0;1;2;2;1;2;1;0]=false);;
assert(lang2 [1;2;0;1;2;1;1;2;1;2;1;2;2;1;1;2;1;2;1;2;2;1;2;1;2;1;1;2]=false);;

let rec lang3 = function
| [] -> true
| 0::1::1::l-> lang3 l
| 1::l -> lang3 l
| _ -> false
;;

assert(lang3 [1;1;1;1;1;1;1;1;1;1;1;1]=true);;
assert(lang3 [1;1;1;1;0;1;0;1;1;1;1;1]=false);;
assert(lang3 [1;1;1;1;0;1;1;0;1;1;1;1]=true);;
assert(lang3 [0;1;1;0;1;1;0;1;1;1;1;1]=true);;
assert(lang3 [0;1;0;1;1;0;1;1;1;1;1;1]=false);;

let rec lang4 = 
  let rec countU = function
    | [] -> 0
    | 1::l -> 1 + countU l
    | 0::l -> 0 + countU l
    | _ -> failwith "not in language"
  in
  let rec countZ = function
    | [] -> 0
    | 0::l -> 1 + countZ l
    | 1::l -> 0 + countZ l
    | _ -> failwith "not in language"
  in 
  function x -> countZ x = countU x || countZ x > countU x
;;

assert(lang4 [1;1;1;0;0;0]=true);;
assert(lang4 [0;1;1;1;0;0;0]=true);;
assert(lang4 [1;1;1;1;0;0;0]=false);;
assert(try lang4 [1;1;1;2;0;0;0] |> fun _ -> false with _ -> true);;
assert(lang4 [1;0;1;1;0;1;0;0;0]=true);;

let rec lang5 = 
  let rec countU = function
    | [] -> 0
    | 1::l -> 1 + countU l
    | 0::l -> 0 + countU l
    | _ -> failwith "not in language"
  in
  let rec countZ = function
    | [] -> 0
    | 0::l -> 1 + countZ l
    | 1::l -> 0 + countZ l
    | _ -> failwith "not in language"
  in 
  function x -> countZ x = countU x
;;

assert(lang5 [1;1;1;0;0;0]=true);;
assert(lang5 [0;1;1;1;0;0;0]=false);;
assert(lang5 [1;1;1;1;0;0;0]=false);;
assert(try lang5 [1;1;1;2;0;0;0] |> fun _ -> false with _ -> true);;
assert(lang5 [1;0;1;1;0;1;0;0;0]=false);;

let lang6 =
  let rec lang6_1 n = function
  | [] -> n=0
  | 1::l -> lang6_1 (n-1) l
  | _ -> false
  in
  let rec lang6_0 n = function
  | [] -> n=0
  | 0::l -> lang6_0 (n+1) l
  | 1::l -> lang6_1 (n-1) l
  | _ -> false
  in
  function x -> lang6_0 0 x
;;

assert(lang6 [0;1]=true);;
assert(lang6 [0;0;0;1;1;1]=true);;
assert(lang6 [0;0;0;0;1;1;1]=false);;
assert(lang6 [0;0;0;1;1;1;1]=false);;
assert(lang6 [0;0;0;1;1;1;0;1]=false);;

let lang7 =
  let rec lang7_1 n = function
    | [] -> n = 0
    | 0 :: l -> lang7_1 (n - 1) l
    | _ -> false
  in
  let rec lang7_0 n = function
    | [] -> false                   
    | 0 :: l -> lang7_0 (n + 1) l   
    | 1 :: l -> lang7_1 n l         
    | _ -> false
  in
  fun x -> lang7_0 0 x
;;

assert(lang7 [1]=true);;
assert(lang7 [0;1;0]=true);;
assert(lang7 [0;0;0;1;0;0;0]=true);;
assert(lang7 [0;0;0;0;0;0;0]=false);;
assert(lang7 [0;0;0;1;0;0]=false);;
assert(lang7 [0;0;1;0;0;0]=false);;

let rec lang8 = function
| [] -> true
| 0::l | 2::l -> lang8 l
| 1::l -> (count 0 l) = (count 2 l) && lang8 l
| _ -> false
;;

assert(lang8 [1;2;0]=true);;
assert(lang8 [2;0;2]=true);;
assert(lang8 [1;2;1;0]=false);;
assert(lang8 [1;2;1;0;2;0]=false);;
assert(lang8 [1;2;0;1;0;2;2;0]=true);;
assert(lang8 [1;2;0;1;0;2;2;0;1;2;0;0;2;2;0;1;2;0;1;0;0;2;2;1;0;2;0;2;0;2;1;2;0;1;0;2;2;0;1;2;0;0;2;2;0;1;2;0;1;0;0;2;2;1;1;2;1;2;1;2]=false);;
assert(lang8 [1;2;0;1;0;2;2;0;1;2;0;0;2;2;0;1;2;0;1;0;0;2;2;1;0;2;0;2;0;2;1;2;0;1;0;2;2;0;1;2;0;0;2;2;0;1;2;0;1;0;0;2;2;1;0;2;0;2;0;2]=true);;
assert(lang8 [2;2;0;1;0;2;2;0;1;2;0;0;2;2;0;1;2;0;1;0;0;2;2;1;0;2;2;2;0;0;2;0;1;0;2;2;0;1;2;0;0;2;2;0;1;2;0;1;0;0;2;2;1;0;2;2;2;0;0;1]=true);;
assert(lang8 [2;2;0;1;0;2;2;0;1;2;0;0;2;2;0;1;2;0;1;0;0;2;2;2;0;0;1;0;2;2;0;1;2;0;0;2;2;0;1;0;0;1;0;0;2;2;1;0;2;2;2;0;0;1;1;1;1;1;1;1]=false);;
assert(lang8 [2;0;0;0;0;0;1;2;0;0;2;2;0;1;2;0;1;0;0;2;2;1;0;2;2;2;0;0;2;2;2;0;0;0;0;0;1;2;0;0;2;2;0;1;2;0;1;0;0;2;2;1;0;2;2;2;0;0;2;2]=false);;
(*Continuo domani*)