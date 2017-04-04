exception Complex

let sqrt n =
  if n < 0 then raise Complex else sqrt_inner 1 n
