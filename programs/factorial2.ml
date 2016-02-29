(* Using two structure items *)
let rec factorial n =
  if n = 1 then 1 else n * factorial (n - 1)

let x =
  factorial 4

