let rec rev lst = 
  match lst with
  | [] -> []
  | h :: t -> (rev t) @ [h];;

let is_palindrome lst = lst = (rev lst);;  

assert(is_palindrome []);;
assert(is_palindrome ['a';'n';'n';'a']);;
assert(is_palindrome ['r';'a';'d';'a';'r']);;
assert(is_palindrome ['a';'n';'n';'e'] = false);;
assert(is_palindrome ['z';'a';'n';'n';'a'] = false);;