type ('a,'b) fsa = {
  trans: ('a * 'b * 'a) list;      (* set of transitions *)
  init: 'a;                        (* initial state *)
  final: 'a list                   (* final states *)
}

(* m1 deterministic and complete *)
let m1 = { 
  trans = [(0,'0',0);(0,'1',1);
           (1,'0',2);(1,'1',2);
           (2,'0',2);(2,'1',2)];
  init = 0;
  final = [1] }
;;

(* m2 non-deterministic and non-complete *)
let m2 = { 
  trans = [(0,'0',0);(0,'0',1);
           (1,'0',2);(1,'1',2);
           (2,'0',2)];
  init = 0;
  final = [1] }
;;

(* m3 deterministic and non-complete *)
let m3 = { 
  trans = [(0,'0',0);(0,'1',1);
           (1,'0',1);(1,'1',2)];
  init = 0;
  final = [1;2] }
;;

let rec subseteq xl yl = match xl with
    [] -> true
  | x::xl' -> List.mem x yl && subseteq xl' yl;;

let seteq xl yl = (subseteq xl yl) && (subseteq yl xl);;

let mkset l = List.fold_left (fun nl vl -> if List.mem vl nl then nl else vl::nl) [] l;;

let getlabels lang =
  let rec gerLabels lst trans =
    match trans with
    | [] -> lst
    | (x,y,z)::h -> gerLabels (y::lst) h
  in
    mkset (gerLabels [] lang.trans)
;;
 
assert (seteq (getlabels m1) ['0';'1']);;
assert (seteq (getlabels m2) ['0';'1']);;
assert (seteq (getlabels m3) ['0';'1']);;