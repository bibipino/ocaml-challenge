type student = {
  id: string;
  name: string;
  surname: string;
  vote: int option;
  laude: bool
}

let alf2023 = [
  { id="60/61/65570"; name="Ambra"; surname="Ambu"; vote=Some 21; laude=false };
  { id="61/61/65778"; name="Brunello"; surname="Brundu"; vote=Some 18; laude=false };
  { id="60/61/65624"; name="Costantino"; surname="Cossu"; vote=Some 24; laude=false };
  { id="60/61/65808"; name="Deborah"; surname="Demurtas"; vote=Some 28; laude=false };
  { id="60/61/65668"; name="Efisio"; surname="Ennas"; vote=Some 18; laude=false };
  { id="60/61/65564"; name="Felicino"; surname="Frau"; vote=None; laude=false };
  { id="60/64/20203"; name="Gavino"; surname="Girau"; vote=Some 20; laude=false };
  { id="60/61/65892"; name="heidi"; surname="hernandez"; vote=Some 8; laude=true };
  { id="60/61/65563"; name="Igino igor"; surname="Ibba"; vote=Some 15; laude=false };
  { id="60/61/64427"; name="Lillo"; surname="Lilliu"; vote=Some 25; laude=false };
  { id="60/61/65448"; name="Morgan"; surname="Murtas"; vote=Some 15; laude=false };
  { id="61/61/65213"; name="Nathan"; surname="Nieddu"; vote=Some 16; laude=false };
  { id="60/61/65832"; name="Ornella"; surname="Onnis"; vote=Some 30; laude=true };
  { id="60/61/65517"; name="Pinuccio"; surname="Puddu"; vote=Some 28; laude=false };
  { id="60/64/21222"; name="Quintilio"; surname="Quaglioni"; vote=Some 22; laude=false };
  { id="60/61/65907"; name="Rihanna"; surname="Ruzzu"; vote=Some 18; laude=false };
  { id="60/61/65766"; name="Samantah"; surname="Sulis"; vote=Some 30; laude=false };
  { id="60/61/65730"; name="Tatiana"; surname="Truzzu"; vote=Some 30; laude=true };
  { id="60/61/65738"; name="Ubaldo"; surname="Urru"; vote=None; laude=true };
  { id="60/61/65722"; name="Valentina"; surname="Vargiu"; vote=Some 30; laude=true };
  { id="60/61/65592"; name="Zlatan"; surname="Zuncheddu"; vote=Some 18; laude = false }
];;

let rec id_of_noshow = function
| [] -> []
| t::h -> if t.vote = None then [t.id] @ id_of_noshow h else id_of_noshow h
;;

assert(id_of_noshow alf2023 = ["60/61/65564";"60/61/65738"]);;

let rec upgradeable = function
| [] -> []
| t :: h -> match t.vote with
| None -> [] @ upgradeable h
| Some x -> (if x >= 15 && x <= 17 then [t.name] else []) @ upgradeable h
;;

assert(upgradeable alf2023 = ["Igino igor";"Morgan";"Nathan"]);;

let rec mem x set = 
    match set with
    | [] -> false
    | t :: h -> x = t || mem x h
;;

let upgrade = 
  let rec checka l1 l2 = match l2 with
  | [] -> l1
  | t::h -> if mem t.name (upgradeable [t]) then 
    let nt = {id=t.id; name=t.name; surname=t.surname; vote=Some 18; laude=t.laude } in
    checka (l1 @ [nt]) h else checka (l1@[t]) h
  in
    function
    | [] -> []
    | x -> checka [] x
;;

let alf2023upgraded = [
  { id="60/61/65570"; name="Ambra"; surname="Ambu"; vote=Some 21; laude=false };
  { id="61/61/65778"; name="Brunello"; surname="Brundu"; vote=Some 18; laude=false };
  { id="60/61/65624"; name="Costantino"; surname="Cossu"; vote=Some 24; laude=false };
  { id="60/61/65808"; name="Deborah"; surname="Demurtas"; vote=Some 28; laude=false };
  { id="60/61/65668"; name="Efisio"; surname="Ennas"; vote=Some 18; laude=false };
  { id="60/61/65564"; name="Felicino"; surname="Frau"; vote=None; laude=false };
  { id="60/64/20203"; name="Gavino"; surname="Girau"; vote=Some 20; laude=false };
  { id="60/61/65892"; name="heidi"; surname="hernandez"; vote=Some 8; laude=true };
  { id="60/61/65563"; name="Igino igor"; surname="Ibba"; vote=Some 18; laude=false };
  { id="60/61/64427"; name="Lillo"; surname="Lilliu"; vote=Some 25; laude=false };
  { id="60/61/65448"; name="Morgan"; surname="Murtas"; vote=Some 18; laude=false };
  { id="61/61/65213"; name="Nathan"; surname="Nieddu"; vote=Some 18; laude=false };
  { id="60/61/65832"; name="Ornella"; surname="Onnis"; vote=Some 30; laude=true };
  { id="60/61/65517"; name="Pinuccio"; surname="Puddu"; vote=Some 28; laude=false };
  { id="60/64/21222"; name="Quintilio"; surname="Quaglioni"; vote=Some 22; laude=false };
  { id="60/61/65907"; name="Rihanna"; surname="Ruzzu"; vote=Some 18; laude=false };
  { id="60/61/65766"; name="Samantah"; surname="Sulis"; vote=Some 30; laude=false };
  { id="60/61/65730"; name="Tatiana"; surname="Truzzu"; vote=Some 30; laude=true };
  { id="60/61/65738"; name="Ubaldo"; surname="Urru"; vote=None; laude=true };
  { id="60/61/65722"; name="Valentina"; surname="Vargiu"; vote=Some 30; laude=true };
  { id="60/61/65592"; name="Zlatan"; surname="Zuncheddu"; vote=Some 18; laude = false }
];;

assert(upgrade alf2023 = alf2023upgraded);;

let rec wrong_laude = function
| [] -> []
| t :: h -> match t.vote,t.laude with
| Some 30, true -> [] @ wrong_laude h
| _ , true -> [t.name] @ wrong_laude h
| _ -> [] @ wrong_laude h
;;

assert(wrong_laude alf2023 = ["heidi";"Ubaldo"]);;

let fix_laude = 
  let rec checka l1 l2 = match l2 with
  | [] -> l1
  | t::h -> if mem t.name (wrong_laude [t]) then 
    let nt = {id=t.id; name=t.name; surname=t.surname; vote=t.vote; laude=false} in
    checka (l1 @ [nt]) h else checka (l1@[t]) h
  in
    function
    | [] -> []
    | x -> checka [] x
;;

let alf2023fixedLaude = [
  { id="60/61/65570"; name="Ambra"; surname="Ambu"; vote=Some 21; laude=false };
  { id="61/61/65778"; name="Brunello"; surname="Brundu"; vote=Some 18; laude=false };
  { id="60/61/65624"; name="Costantino"; surname="Cossu"; vote=Some 24; laude=false };
  { id="60/61/65808"; name="Deborah"; surname="Demurtas"; vote=Some 28; laude=false };
  { id="60/61/65668"; name="Efisio"; surname="Ennas"; vote=Some 18; laude=false };
  { id="60/61/65564"; name="Felicino"; surname="Frau"; vote=None; laude=false };
  { id="60/64/20203"; name="Gavino"; surname="Girau"; vote=Some 20; laude=false };
  { id="60/61/65892"; name="heidi"; surname="hernandez"; vote=Some 8; laude=false };
  { id="60/61/65563"; name="Igino igor"; surname="Ibba"; vote=Some 15; laude=false };
  { id="60/61/64427"; name="Lillo"; surname="Lilliu"; vote=Some 25; laude=false };
  { id="60/61/65448"; name="Morgan"; surname="Murtas"; vote=Some 15; laude=false };
  { id="61/61/65213"; name="Nathan"; surname="Nieddu"; vote=Some 16; laude=false };
  { id="60/61/65832"; name="Ornella"; surname="Onnis"; vote=Some 30; laude=true };
  { id="60/61/65517"; name="Pinuccio"; surname="Puddu"; vote=Some 28; laude=false };
  { id="60/64/21222"; name="Quintilio"; surname="Quaglioni"; vote=Some 22; laude=false };
  { id="60/61/65907"; name="Rihanna"; surname="Ruzzu"; vote=Some 18; laude=false };
  { id="60/61/65766"; name="Samantah"; surname="Sulis"; vote=Some 30; laude=false };
  { id="60/61/65730"; name="Tatiana"; surname="Truzzu"; vote=Some 30; laude=true };
  { id="60/61/65738"; name="Ubaldo"; surname="Urru"; vote=None; laude=false };
  { id="60/61/65722"; name="Valentina"; surname="Vargiu"; vote=Some 30; laude=true };
  { id="60/61/65592"; name="Zlatan"; surname="Zuncheddu"; vote=Some 18; laude = false }
];;

assert(fix_laude alf2023 = alf2023fixedLaude);;

let percent_passed = 
  let rec passed l =
    match l with
    | [] -> 0
    | t :: h -> match t.vote with
    | None -> 0 + passed h
    | Some x -> (if x>=18 then 1 else 0) + passed h
  in
    function
    | [] -> 0
    | x -> int_of_float (100. *. (float_of_int (passed x)) /. (float_of_int (List.length x)))
;;

assert(percent_passed(fix_laude alf2023) = 71);;
assert(percent_passed(fix_laude (upgrade alf2023)) = 85);;

let avg_vote = 
  let rec passed l =
    match l with
    | [] -> 0
    | t :: h -> match t.vote with
    | None -> 0 + passed h
    | Some x -> x + passed h
  in
    function
    | [] -> 0.
    | x -> (float_of_int (passed x)) /. (float_of_int (List.length x))
;;

avg_vote (fix_laude alf2023);; (*19.7142857142857153*)
avg_vote (fix_laude (upgrade alf2023));; (*20.0952380952380949*)

