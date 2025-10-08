let rec gcd a b = if a = b then a else if a > b then gcd (a-b) b else gcd a (b-a);; (*Non so perche' funzioni ma mi va bene*)

assert(gcd 24 18 = 6);;
assert(gcd 252 105 = 21);;