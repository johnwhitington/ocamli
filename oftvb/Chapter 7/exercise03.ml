let rec sqrt_inner x n =
  if x * x > n then x - 1 else sqrt_inner (x + 1) n
