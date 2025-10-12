type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;

let rec lttree n t comp = match t with
| Empty -> true
| Node (v,l,r) -> if (comp n v) >= 0 then false else
    (lttree n l comp) && (lttree n r comp)
;;

let rec gttree n t comp = match t with
| Empty -> true
| Node (v,l,r) -> if (comp n v) <= 0 then false else
    (gttree n l comp) && (gttree n r comp)
;;

let is_btree t comp = match t with
| Empty -> true
| Node (v,l,r) -> lttree v l comp && gttree v r comp
;;

let rec search t comp n = match t with
| Empty -> false
| Node (v,l,r) -> 
  match comp v n with
  | 0 -> true
  | b when b < 0 -> search l comp n
  | _ -> search r comp n
;;