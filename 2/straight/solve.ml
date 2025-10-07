type suit = S | H | D | C;;
type card = Card of int * suit;;

let rnd_suit () = match Random.int(4) with
    0 -> S
  | 1 -> H
  | 2 -> D
  | 3 -> C
  | _ -> failwith("WTF");;

let rnd_card () = Card(1 + Random.int(10), rnd_suit());;

let rnd_hand () = (rnd_card (),rnd_card (),rnd_card (),rnd_card (),rnd_card ());;

let straight (Card(c1,s1),Card(c2,s2),Card(c3,s3),Card(c4,s4),Card(c5,s5)) : bool =
  let m = min c1 (min c2 (min c3 (min c4 c5))) in
  let pSc minim n (n1,n2,n3,n4,n5) =
    n1-minim = n || n2-minim = n || n3-minim = n || n4-minim = n || n5-minim = n 
  in pSc m 1 (c1,c2,c3,c4,c5) &&
    pSc m 2 (c1,c2,c3,c4,c5) &&
    pSc m 3 (c1,c2,c3,c4,c5) &&
    pSc m 4 (c1,c2,c3,c4,c5)
;;

straight (rnd_hand());;
straight (Card(5,S),Card(4,H),Card(7,D),Card(6,C),Card(3,C));;
straight (Card(5,S),Card(4,H),Card(7,D),Card(6,C),Card(8,C));;
straight (Card(5,S),Card(4,H),Card(7,D),Card(6,C),Card(2,C));;

let test tot = 
  let rec test_rec n tot w =
    if n = 0 then 
      string_of_float (100. *. (float_of_int w /. float_of_int tot)) ^ "%"
    else
      test_rec (n-1) tot (w + if straight(rnd_hand()) then 1 else 0)
  in
  test_rec tot tot 0;;

test 10000;;