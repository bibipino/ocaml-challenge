let hot () = if Random.int(2) = 0 then "head" else "tail";; (*In poche parole () si chiama unit ed e' pari al void*)

let hot_wrong = if Random.int(2) = 0 then "head" else "tail";;