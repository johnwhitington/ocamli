let rec sum n =
  if n = 1 then 1 else n + sum (n - 1)
