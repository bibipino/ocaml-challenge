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
  match comp n v with
  | 0 -> true
  | b when b < 0 -> search l comp n
  | _ -> search r comp n
;;

let t0 =
Node(7,
  Node(4,
    Node(1,Empty,Empty),
    Node(5,Empty,Empty)),
  Node(10,Empty,Empty))
;;

let t1 =
Node(8,
  Node(4,
    Node(1,Empty,Empty),
    Node(5,Empty,Empty)),
  Node(10,Empty,Empty))
;;

gttree 11 t0 compare;;

is_btree t0 compare;; (*Non dovrebbe dare false ci lavoro poi*)

search t0 compare 1;;

compare t0 t1;;