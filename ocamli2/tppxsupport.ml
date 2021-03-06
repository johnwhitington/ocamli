(* [(fun x -> x + 1)] not considered a "value" in Type.t, but does end
 * evaluation in Eval.eval_full. But we always need a heap value in the TPPX. *)
open Type

(* Read environment variables from OCAMLI2PARAM etc. *)
let init () =
  match Sys.getenv_opt "OCAMLI2PARAM" with
    Some "rules" -> Eval.showrules := true
  | _ -> ()

(* At the moment, this is copy-and-pasted mostly from read.ml, because of a compilation order problem read <--> eval *)
let rec to_ocaml_heap_value expr =
  match expr.e with
    Value x -> x
  | ArrayExpr arr ->
      (* This arrayexpr contains only values. Turn it into a value itself. *)
      let x = Obj.new_block 0 (Array.length arr) in
        for i = 0 to Array.length arr - 1 do
          Obj.set_field x i (to_ocaml_heap_value arr.(i))
        done;
        x
  | Cons ({e = Value h}, t) ->
      (* This list now contains only values. Turn it into a value itself. *)
      let cell = Obj.new_block 0 2 in
        Obj.set_field cell 0 h;
        Obj.set_field cell 1 (to_ocaml_heap_value t);
        cell
  | Function _ -> Eval.make_native expr.lets expr
  | _ -> failwith "to_ocaml_heap_value: unknown"

let eval_full env x =
  let program = (Marshal.from_string x 0 : Type.t) in
    match Eval.eval_full !env program with
      {Type.e = Type.Value x} -> x
    | x -> to_ocaml_heap_value x

let eval_full_from_typedtree env x =
  let typedtree = (Marshal.from_string x 0 : Typedtree.expression) in
    let program = Read.finaltype_of_expression !env typedtree in
      match Eval.eval_full !env program with
        {Type.e = Type.Value x} -> x
      | x -> to_ocaml_heap_value x

let addenv envref printas n v t =
  let binding =
    {Type.e = Type.Value v;
     Type.typ = Read.debug_type (Read.remove_links (Marshal.from_string t 0 : Types.type_expr));
     Type.lets = [];
     Type.peek = None;
     Type.printbefore = None;
     Type.printafter = None;
     Type.printas = printas}
  in
    envref := (false, ref [(n, binding)])::!envref
