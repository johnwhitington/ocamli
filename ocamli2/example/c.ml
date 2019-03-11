(* This will be added by the PPX, eventually. We need it here so the TPPX can call it correctly to implement a [@interpret] annotation. *)
let eval_full = Eval.eval_full 

let x = 1 + 2 * 3 [@interpret]

let _ =
  Printf.printf "Result is %i\n" x;
  exit 0

