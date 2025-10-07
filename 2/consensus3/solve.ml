let cns ((f1 : 'a -> 'b), (f2 : 'a -> 'b), (f3 : 'a -> 'b)) : 'a -> 'b option =
  fun x ->
  let safe f = 
    try Some (f x) with _ -> raise Exit
  in
    try 
    let s1 = safe f1 in
    let s2 = safe f2 in
    let s3 = safe f3 in
    match s1,s2,s3 with
    | Some v1, Some v2, Some v3 -> if v1 = v2 || v1 = v3 then Some v1 else if v2 = v3 then Some v2 else None
    | _ , _ , _  -> None
  with Exit -> raise (Failure "undefined")
;;

assert(cns ((fun x -> x),(fun y -> y+4),(fun z -> 5/z)) 1 = Some 5);;
assert(cns ((fun x -> x),(fun y -> y+4),(fun z -> 5/z)) 2 = Some 2);;
assert(cns ((fun x -> x),(fun y -> y+4),(fun z -> 5/z)) 3 = None);;
assert(try cns ((fun x -> x),(fun y -> y+4),(fun z -> 5/z)) 0 |> ignore; false with _ -> true);;
