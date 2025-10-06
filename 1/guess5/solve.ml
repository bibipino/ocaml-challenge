let guess (n : int) = 
  assert(n>=1&&n<=5);
  let r = Random.int(5)+1 in
  (n == r, r);;