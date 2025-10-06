let seven_eleven_doubles () = 
  let d1 = Random.int(6)+1 in 
  let d2 = Random.int(6)+1 in
  ( d1+d2=7 || d1+d2=11 || d1=d2 ,d1,d2);;