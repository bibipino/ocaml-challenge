type winner = Player | Computer | Tie ;;

let win (ph,pg) = 
  let ch = Random.int(5)+1 in
  let cg = Random.int(10)+1 in
  if cg = pg then ((ch,cg), Tie) else
    let sum = ph + ch in
    if pg = sum then ((ch,cg), Player) else
      if cg = sum then ((ch,cg), Computer) else ((ch,cg), Tie);;