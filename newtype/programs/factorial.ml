(* Recursive function of one argument. *)
let rec factorial n =
  if [@show] n = 1 then 1 else n * factorial (n - 1)
in
  factorial 3

