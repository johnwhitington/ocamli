(* This will be added by the PPX, eventually. We need it here so the TPPX can
 * call it correctly to implement a [@interpret] annotation. *)
let env = ref []

let template_string = ""

let template_int = 0

let _ = Print.showvals := false

let eval_full x =
  let program = (Marshal.from_string x 0 : Type.t) in
  match Eval.eval_full !env program with
    {Type.e = Type.Value x} -> x
  | _ -> failwith "eval template: eval_full did not return a value"

let addenv envref n v t =
  let binding =
    {Type.e = Type.Value v;
     Type.typ = (Marshal.from_string t 0 : Types.type_desc);
     Type.lets = [];
     Type.peek = None;
     Type.printas = None}
  in
    envref := (false, ref [(n, binding)])::!envref


(* ------------- The actual text of the file ---------------- *)

let x =
  let y = 1 - 0 in
    addenv env "y" (Obj.magic y : Obj.t) "";   (* At runtime, adds the final value of 'y' *)
    (y + 2 * 3 [@interpret]) + 4

let _ =
  Printf.printf "Result is %i\n" x;
  exit 0

