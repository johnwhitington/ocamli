(* This will be added by the PPX, eventually. We need it here so the TPPX can call it correctly to implement a [@interpret] annotation. *)
let eval_full x =
  Eval.eval_full [] (Read.finaltype_of_typedtree_repr_of_finaltype x)

let x = 1 + 2 * 3 [@interpret]

let _ =
  Printf.printf "Result is %i\n" x;
  exit 0

