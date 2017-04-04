exception Complex

let rec sqrt_inner x n =
  if x * x > n then x - 1 else sqrt_inner (x + 1) n

let sqrt n =
  if n < 0 then raise Complex else sqrt_inner 1 n

let safe_sqrt n =
  try sqrt n with Complex -> 0
