open Typedtree
open Ocamli2type
open Types

let string_of_ocaml_type = function
  Tvar (Some x) -> x
| Tvar None -> "_"
| Tarrow (_, _, _, _) -> "Tarrow"
| Ttuple _ -> "Ttuple"
| Tconstr (path, _, _) -> "Tconstr " ^ Path.name path
| Tobject (_, _) -> "Tobject"
| Tfield (_, _, _, _) -> "Tfield"
| Tnil -> "Tnil"
| Tlink _ -> "Tlink"
| Tsubst _ -> "Tsubst"
| Tvariant _ -> "Tvariant"
| Tunivar _ -> "Tunivar"
| Tpoly (_, _) -> "Tpoly"
| Tpackage (_, _, _) -> "Tpackage"

let op_of_text = function
  "+" | "+." -> Add
| "-" | "-." -> Sub
| "*" | "*." -> Mul
| "/" | "/." -> Div
| _ -> failwith "op_of_text"

(* If an ArrayExpr contains only things which are values, we need to identify
 * it and turn it into a heap object. However, it cannot be considered really a
 * "value", or it would never get evaluted. E.g [|1 + 2; 3|] is an ArrayExpr,
 * but not (yet) a value *)
let rec array_expr_should_be_value arr =
  Array.for_all
    (function {e = ArrayExpr a} -> array_expr_should_be_value a
            | x -> is_value x)
    arr

let rec to_ocaml_heap_value = function
  Value x -> x
| ArrayExpr arr ->
    (* This arrayexpr contains only values. Turn it into a value itself. *)
    let x = Obj.new_block 0 (Array.length arr) in
      for i = 0 to Array.length arr - 1 do
        Obj.set_field x i (to_ocaml_heap_value arr.(i).e)
      done;
      x
| _ -> failwith "to_ocaml_heap_value: unknown"

exception IsImplicitLet of string * Ocamli2type.t * Ocamli2type.t 

let rec finaltype_of_expression_desc = function
  Texp_constant (Const_int x) -> Value (Obj.repr x)
| Texp_constant (Const_float x) -> Value (Obj.repr (float_of_string x))
| Texp_ident (p, _, _) -> Var (Path.name p)
| Texp_let (_, [binding], e) ->
    let (var, expr) = finaltype_of_binding binding
    and expr' = finaltype_of_expression e in
      if is_value expr then
        raise (IsImplicitLet (var, expr, expr'))
      else
        Let ((var, expr), expr')
| Texp_apply
    ({exp_desc =
        Texp_ident (Path.Pdot (Path.Pident i, (("+" | "-" | "*" | "/") as optext)), _, _)},
     [(_, Some arg1); (_, Some arg2)]) when Ident.name i = "Stdlib" ->
       IntOp (op_of_text optext, finaltype_of_expression arg1, finaltype_of_expression arg2)
| Texp_apply
    ({exp_desc =
        Texp_ident (Path.Pdot (Path.Pident i, (("+." | "-." | "*." | "/.") as optext)), _, _)},
     [(_, Some arg1); (_, Some arg2)]) when Ident.name i = "Stdlib" ->
       FOp (op_of_text optext, finaltype_of_expression arg1, finaltype_of_expression arg2)
| Texp_apply
    ({exp_desc =
      Texp_ident (Path.Pdot (Path.Pdot (Path.Pident x, y), z), _, _)},
        [(_, Some arr); (_, Some index)])
      when Ident.name x = "Stdlib" && y = "Array" && z = "get" ->
        ArrayGet (finaltype_of_expression arr, finaltype_of_expression index)
| Texp_apply
    ({exp_desc =
      Texp_ident (Path.Pdot (Path.Pdot (Path.Pident x, y), z), _, _)},
        [(_, Some arr); (_, Some index); (_, Some newval)])
      when Ident.name x = "Stdlib" && y = "Array" && z = "set" ->
        ArraySet
          (finaltype_of_expression arr,
           finaltype_of_expression index,
           finaltype_of_expression newval)
| Texp_array es ->
    let arr = Array.of_list (List.map finaltype_of_expression es) in
      if array_expr_should_be_value arr then
        Value (to_ocaml_heap_value (ArrayExpr arr))
      else
        ArrayExpr arr
| _ -> failwith "finaltype_of_expression_desc: unknown"

and finaltype_of_binding {vb_pat; vb_expr} =
  let var = match vb_pat with
    {pat_desc = Tpat_var (i, _)} -> Ident.name i
  | _ -> failwith "finaltype_of_binding: pattern not supported"
  in
    (var, finaltype_of_expression vb_expr)

and finaltype_of_expression exp =
  try
    {e = finaltype_of_expression_desc exp.exp_desc;
     typ = find_type_desc exp.exp_type;
     lets = []}
  with
    IsImplicitLet (var, expr, expr') ->
      (*Printf.printf "Adding implicit let %s\n" var;*)
      {expr' with lets = (var, expr) :: expr'.lets}

(* For now just first structure item. To remove later when we have real structure item support. *)
let finaltype_of_typedtree {str_items} =
  match (List.hd str_items).str_desc with
    Tstr_value (_, [vb]) -> finaltype_of_expression vb.vb_expr
  | Tstr_eval (e, _) -> finaltype_of_expression e
  | _ -> failwith "finaltype_of_typedtree"

let env =
  Compmisc.init_path false;
  Compmisc.initial_env ()

let typedtree_of_string ?(filename="") code =
  let ast =
    let lexer = Lexing.from_string code in
    Location.init lexer filename;
    Parse.implementation lexer
  in
    try
      let typedtree, _ = Typemod.type_implementation "foo.ml" "" "example" env ast in
      typedtree
    with
      e ->
        Location.report_exception Format.std_formatter e;
        exit 2
