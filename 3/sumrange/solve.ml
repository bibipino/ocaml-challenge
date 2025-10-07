let sumrange min max = 
  if min > max then 0 else
    let rec sum min max s =
      if min = max then s + max else
        sum min (max - 1) (s + max)
      in 
        sum min max 0
;;

assert (sumrange 0 1 = 1);;
assert (sumrange 1 3 = 6);;
assert (sumrange 3 2 = 0);;