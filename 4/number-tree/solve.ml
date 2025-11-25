type 'a btree = Empty | Node of 'a * 'a btree * 'a btree;;

let number_tree t = 
  let rec ntree n = function
  | Empty -> (Empty, n)
  | Node (a,b,c) -> 
    let (b1, n2) = ntree n b in
    let n1 = n2 in
    let (c1, n3) = ntree (n2 + 1) c in
    (Node ((n1, a), b1, c1), n3) in 
  fst (ntree 0 t)
;;

let t = Node("d", Node("c", Node("a", Empty, Node("b",Empty,Empty)), Empty), Node("e",Empty,Empty));;

assert (number_tree t =
  Node((3,"d"),
    Node((2,"c"),
      Node((0,"a"),
        Empty,
        Node((1,"b"),Empty,Empty)),
    Empty),
  Node((4,"e"),Empty,Empty)));;