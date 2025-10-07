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

let poker4 (Card(n1,s1),Card(n2,s2),Card(n3,s3),Card(n4,s4)) : bool =
  n1 = n2 && n2 = n3 && n3 = n4 && s1<>s2 && s1<>s3 && s1<>s4 && s2<>s3 && s2<>s4 && s3<>s4;;

let poker (c1,c2,c3,c4,c5) = 
  poker4(c1,c2,c3,c4) ||
  poker4(c1,c2,c3,c5) ||
  poker4(c1,c2,c4,c5) ||
  poker4(c1,c3,c4,c5) ||
  poker4(c2,c3,c4,c5);;

poker (rnd_hand());;
poker (Card(1,S),Card(1,H),Card(1,D),Card(1,C),Card(2,S));;
poker (Card(2,S),Card(1,H),Card(1,D),Card(1,C),Card(1,S));;
poker (Card(1,S),Card(1,H),Card(1,D),Card(2,C),Card(2,S));;
poker (Card(1,S),Card(1,H),Card(1,D),Card(1,H),Card(2,S));;

let test tot =
  let rec test_rec n tot w = 
    if n=0 then 
      string_of_float(100. *. (float_of_int w /. float_of_int tot)) ^ "%" 
    else 
      test_rec (n-1) tot (w + if poker (rnd_hand()) then 1 else 0)
  in test_rec tot tot 0;
;;

test 100000;;