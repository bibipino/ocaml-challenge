let rec mem x set = 
    match set with
    | [] -> false
    | t :: h -> x = t || mem x h
;;

assert(mem 1 [1;3;5]);;
assert(mem 2 [1;3;5] = false);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
assert(mem [1;2] [[1];[2];[1;2]]);;

let rec subseteq l1 l2 =
  match l1 with
  [] -> true
  | t :: h -> (mem t l2) && subseteq h l2
;;

assert(subseteq [] [1;3;5]);;
assert(subseteq [1;5] [5;1]);;
assert(subseteq [1;5] [1;3;5]);;
assert(subseteq [1;5] [5;3;1]);;
assert(subseteq [2] [1;3;5] = false);;
assert(subseteq [[1;2]] [[1];[2];[2;1]] = false);;
assert(subseteq [[1];[2;1]] [[1];[2];[2;1]]);;

let seteq l1 l2 = List.length l1 = List.length l2 && subseteq l1 l2;;

assert(seteq [1;5;3] [1;3;5]);;
assert(seteq [1;5;2] [1;3;5] = false);;
assert(seteq [[1;2]] [[2;1]] = false);;
assert(seteq [[1];[1;2]] [[1;2];[1]]);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;

let rec dup = function
  [] -> false
  | t :: h -> (subseteq [t] h) || dup h
;;

assert(dup [] = false);;
assert(dup [1;1]);;
assert(dup [1;3;5] = false);;
assert(dup [1;3;5;3]);;

let rec mkset = function
 [] -> []
 | t :: h -> if subseteq [t] h then mkset h else t :: mkset h
;;

assert(seteq (mkset [1;2;3;2;1]) [1;2;3]);;
assert(seteq (mkset [1;2;1;2;1]) [1;2]);;
assert(seteq (mkset [1;2;3]) [2;3;1]);;

let rec union l1 l2 =
  mkset (l1 @ l2)
;;

assert(seteq (union [1;2;3] []) [1;2;3]);;
assert(seteq (union [] [2;3;4]) [2;3;4]);;
assert(seteq (union [1;2;3] [2;3;4]) [1;2;3;4]);;

let rec inter l1 l2 =
  match l1 with
  [] -> []
  | t :: h -> if subseteq [t] l2 then t :: inter h l2 else inter h l2
;;

assert(seteq (inter [1;2;3] []) []);;
assert(seteq (inter [] [2;3;4]) []);;
assert(seteq (inter [1;2;3] [2;3;4]) [2;3]);;

let rec diff l1 l2 =
  match l1 with
  [] -> []
  | t :: h -> if mem t l2 then diff h l2 else t :: diff h l2
;;

assert(seteq (diff [1;2;3] []) [1;2;3]);;
assert(seteq (diff [] [2;3;4]) []);;
assert(seteq (diff [1;2;3] [2;3;4]) [1]);;
assert(seteq (diff [1;2;3] [3;1]) [2]);;

let rec dsum l1 l2 =
  match l1, l2 with
  | [],[] -> []
  | [], t::h  -> (1,t) :: dsum l1 h
  | t::h, _ -> (0,t) :: dsum h l2
;;

assert(seteq (dsum [1;2;3] []) [(0,1);(0,2);(0,3)]);;
assert(seteq (dsum [] [2;3;4]) [(1,2);(1,3);(1,4)]);;
assert(seteq (dsum [1;2] [2;3]) [(0,1);(0,2);(1,2);(1,3)]);;

let rec powset = function
  [] -> [[]]
  | t:: h -> let pows = powset h in pows @ List.map (fun h -> union [t] h) pows
;; (*Questo e' molto wawawawawa quindi bho spero di non rivedere un powerset mai piu' in tutta la mia vita*)

assert (powset [] = [[]]);;
assert (seteq (powset [1]) [[];[1]]);;
assert (List.length (powset [1;2]) = 4);;
assert (List.length (powset [1;2;3]) = 8);;
assert (List.length (powset [1;2;3;4]) = 16);;