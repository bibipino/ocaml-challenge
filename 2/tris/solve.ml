let tris (a,b,c,d) : bool = 
  if (a = b && a = c) || (a = c && a = d) || (a = b && a = d) then true else (b = c && c = d);;

let hand () = (Random.int(10)+1,Random.int(10)+1,Random.int(10)+1,Random.int(10)+1);;

assert(tris (1,1,1,1) = true);;
assert(tris (1,1,1,2) = true);;
assert(tris (1,1,2,1) = true);;
assert(tris (1,1,2,2) = false);;
assert(tris (1,2,1,1) = true);;
assert(tris (1,2,1,2) = false);;
assert(tris (1,2,2,1) = false);;
assert(tris (1,2,2,2) = true);;
assert(tris (2,1,1,1) = true);;
assert(tris (2,1,1,2) = false);;
assert(tris (2,1,2,1) = false);;
assert(tris (2,1,2,2) = true);;
assert(tris (2,2,1,1) = false);;
assert(tris (2,2,1,2) = true);;
assert(tris (2,2,2,1) = true);;
assert(tris (2,2,2,2) = true);;