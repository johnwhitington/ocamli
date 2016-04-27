(* Recursive function of one argument. *)
let rec factorial n =
  if n = 1 then 1 else (n * factorial (n - 1)) [@debug]
in
  factorial 4

