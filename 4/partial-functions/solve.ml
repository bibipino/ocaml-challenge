let _A0 = [1;2;3];;
let _B0 = [2;3;4];;

let f1 = [(1,2);(2,3);(1,4)]
and f2 = [(1,2);(2,3);(4,4)]
and f3 = [(1,2);(2,3);(3,5)]
and f4 = [(1,2);(2,3);(3,4)]
and f5 = [(1,2);(2,3)]
and f6 = [(1,2);(2,3);(3,2)];;

let rec dup = function
| [] -> false
| t::h -> List.mem t h
;;

let is_fun _A _B f =
  List.for_all (fun (a,b) -> List.mem a _A && List.mem b _B) f && not (dup (List.map fst f))
;;

assert (not (is_fun _A0 _B0 f1));;
assert (not (is_fun _A0 _B0 f2));;
assert (not (is_fun _A0 _B0 f3));;
assert (is_fun _A0 _B0 f4);;
assert (is_fun _A0 _B0 f5);;
assert (is_fun _A0 _B0 f6);;

let is_tot _A _B f =
  is_fun _A _B f && List.length f = List.length _A
;;

assert (is_tot _A0 _B0 f4);;
assert (not (is_tot _A0 _B0 f5));;
assert (is_fun _A0 _B0 f6);;

let is_inj _A _B f =
  is_fun _A _B f && not (dup (List.map snd f))
;;

assert (is_inj _A0 _B0 f4);;
assert (is_inj _A0 _B0 f5);;
assert (not (is_inj _A0 _B0 f6));;

let is_surj _A _B f =
  is_fun _A _B f && List.for_all (fun b -> List.mem b (List.map snd f)) _B
;;

assert (is_surj _A0 _B0 f4);;
assert (not (is_surj _A0 _B0 f5));;
assert (not (is_surj _A0 _B0 f6));;

let rec rel_of_fun _A f = match _A with
| [] -> []
| t::h -> [(t,f t)] @ rel_of_fun h f 
;;

let g1 = function
    1 -> 2
  | 2 -> 3
  | 3 -> 5
  | _ -> failwith "undefined"
;;

assert(rel_of_fun [1;2;3] g1 = [(1, 2); (2, 3); (3, 5)]);;

let bot = function
| x -> failwith "undefined"

let bind f a b= function
| x-> if x=a then b else f x
;;

let rec fun_of_rel = function
| [] -> bot
| (ta,tb)::h -> let f = fun_of_rel h in bind f ta tb
;;

let is_mono f =
  let rec is_dec = function
  | [] -> true
  | [x] -> true
  | t1::t2::h -> t1>=t2 && is_dec (t2::h)
  in
  let rec is_crs = function
  | [] -> true
  | [x] -> true
  | t1::t2::h -> t1<=t2 && is_crs (t2::h)
  in
  let nl = List.sort (fun (a,b) (c,d) -> compare a c) f
  in
  is_dec (List.map snd nl) || is_crs (List.map snd nl)
;;

assert (is_mono f4);;
assert (is_mono f5);;
assert (not (is_mono f6));;