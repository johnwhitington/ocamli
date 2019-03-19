let eval_full env x =
  let program = (Marshal.from_string x 0 : Type.t) in
  match Eval.eval_full !env program with
    {Type.e = Type.Value x} -> x
  | _ -> failwith "eval template: eval_full did not return a value"

let addenv envref n v t =
  let binding =
    {Type.e = Type.Value v;
     Type.typ = (Marshal.from_string t 0 : Types.type_expr);
     Type.lets = [];
     Type.peek = None;
     Type.printas = None}
  in
    envref := (false, ref [(n, binding)])::!envref
