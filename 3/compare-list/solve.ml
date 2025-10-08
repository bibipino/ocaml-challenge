let rec compare_list cmp l1 l2 = 
  match l1,l2 with
  [],[] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x1::l1, x2::l2 -> if cmp x1 x2 then 1 else 
    if cmp x2 x1 then (-1) else
      compare_list cmp l1 l2;;  
(*non funziona perche' queste funzioni fanno int -> int -> int invece di int -> int -> bool 
  e noi vogliamo funzioni che prendano 'a -> 'a -> bool*)

let rec cmplst cmp l1 l2 = 
  match l1,l2 with
  [],[] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x1::l1, x2::l2 -> if cmp x1 x2 = 0 then cmplst cmp l1 l2 else 
    cmp x1 x2
;;

assert (cmplst Int.compare [0] [1] <= 0);;
assert (cmplst Int.compare [0;1;0;0] [0;1;0;0] = 0);;
assert (cmplst Int.compare [0;1;0;0] [0;0;1;0] >= 0);;
assert (cmplst Int.compare [1;1;1;1] [1] >= 0);;
assert (cmplst Int.compare [1;1;2;3] [1;2;3] <= 0);;
assert (cmplst Int.compare [2;42;1] [3] <= 0);;
assert (cmplst Char.compare ['a';'b';'a';'c';'o'] ['a';'b';'e';'t';'e'] <= 0);;