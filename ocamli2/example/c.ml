(* This will be added by the PPX, eventually. We need it here so the TPPX can call it correctly to implement a [@interpret] annotation. *)
let eval_full x =
  match Eval.eval_full [] (Marshal.from_string x 0 : Type.t) with
    {Type.e = Type.Value x} -> x
  | _ -> failwith "eval template: eval_full did not return a value"

let template_string = ""

let x = (1 + 2 * 3 [@interpret]) + 4

let _ =
  Printf.printf "Result is %i\n" x;
  exit 0

