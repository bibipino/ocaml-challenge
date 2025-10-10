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

let dup l = List.length l <> List.length (mkset l);;

let getlabels lang =
  let rec genLabels lst trans =
    match trans with
    | [] -> lst
    | (x,y,z)::h -> genLabels (y::lst) h
  in
    mkset (genLabels [] lang.trans)
;;
 
assert (seteq (getlabels m1) ['0';'1']);;
assert (seteq (getlabels m2) ['0';'1']);;
assert (seteq (getlabels m3) ['0';'1']);;

let outlabels m q =
  let rec genOutLabels lst m trans =
    match trans with
    | [] -> lst
    | (x,y,z)::h -> if x=m then genOutLabels (y::lst) m h else genOutLabels lst m h
  in
    mkset (genOutLabels [] q m.trans)
;;

assert (seteq (outlabels m1 0) ['0';'1']);;
assert (seteq (outlabels m1 1) ['0';'1']);;
assert (seteq (outlabels m1 2) ['0';'1']);;
assert (seteq (outlabels m2 2) ['0']);;

let getstates lang =
  let rec genLabels lst trans =
    match trans with
    | [] -> lst
    | (x,y,z)::h -> genLabels (x::z::lst) h
  in
    List.sort (fun x y -> compare x y) (mkset (genLabels [] lang.trans))
;;

assert (seteq (getstates m1) [0;1;2]);;
assert (seteq (getstates m2) [0;1;2]);;
assert (seteq (getstates m3) [0;1;2]);;

let is_complete m =
  let ql = getstates m in
  let ll = getlabels m in
  List.for_all (fun x -> seteq (outlabels m x) ll) ql
;;

assert (is_complete m1);;
assert (is_complete m2 = false);;
assert (is_complete m3 = false);;

let is_deterministic m =
  not (dup (List.map (fun (x,y,z) -> (x,y)) m.trans))
;;

assert (is_deterministic m1);;
assert (is_deterministic m2 = false);;
assert (is_deterministic m3);;

let step1 q a m =
  let rec step1_rec q a = function
  | [] -> failwith "nope"
  | (x,y,z)::h -> if x=q && y=a then z else step1_rec q a h
  in
    step1_rec q a m.trans
;;

assert (step1 0 '0' m1 = 0);;
assert (step1 0 '1' m1 = 1);;
assert (step1 1 '0' m1 = 2);;
assert (step1 1 '1' m1 = 2);;
assert (step1 2 '0' m1 = 2);;
assert (step1 2 '1' m1 = 2);;

let rec step q a m = 
  match a with
  | [] -> q
  | t::h -> step (step1 q t m) h m
;;

assert(step 0 ['0';'0';'0'] m1 = 0);;
assert(step 0 ['0';'1';'1'] m1 = 2);;

let accept a m = List.mem (step m.init a m) m.final;;

assert (accept ['0';'0';'1'] m1);;
assert (accept ['0';'0';'1';'1'] m1 = false);;
assert (accept ['1';'0';'0';'1'] m1 = false);;

(*Non ho capito cosa voglia lol*)

(*Analizza il codice*)
let complete m sink =
  let qQ = getstates m in
  let lL = getlabels m in
  let tl = List.fold_left (fun tl q -> tl @ (List.map (fun a -> (q,a,sink)) lL)) [] (sink::qQ) in
  let sl = List.filter (fun (q,a,_) -> not (List.mem a (outlabels m q))) tl
  in { trans = m.trans @ sl; init = m.init; final = m.final }
;;

let m3' = complete m3 3;;
assert (is_complete m3');;
assert (accept ['0';'1';'0';'1'] m3');;
assert (accept ['0';'0';'1';'0';'0'] m3');;
assert (accept ['0';'1';'1';'0'] m3' = false);;