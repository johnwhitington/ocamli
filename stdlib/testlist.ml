let f x = x + 1

let rec sum x =
  if x = 0 then f 0 else 1 + sum (x - 1)

