let has_one n = 
  if n<0 then failwith("wawa") else
  let rec div_ten n e =
    if n < 10 then e || n mod 10 = 1 else 
      div_ten (n/10) (e || n mod 10 = 1)
    in
  div_ten n false
;;

assert(has_one 10 = true);;
assert(has_one 220 = false);;
assert(has_one 911 = true);;
assert(has_one 451 = true);;
assert(try has_one (-10) |> fun _ -> false with _ -> true);;
