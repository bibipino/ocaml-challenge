let (<|>) (x : 'a option) (y : 'a option) : 'a option = 
  match (x,y) with
  | (Some x, _) -> Some x
  | (_, Some y) -> Some y
  | (_ , _) -> None
;;


assert (None <|> Some false = Some false);;
assert (Some true <|> None <|> Some false = Some true);;
assert (Some 3 <|> None = Some 3);;
assert (Some "cat" <|> Some "dog" = Some "cat");;
assert (None <|> None <|> Some "dog" <|> None = Some "dog");;