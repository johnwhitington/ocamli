open Typedtree
open Ocamli2type
open Types


let op_of_text = function
  "+" | "+." -> Add
| "-" | "-." -> Sub
| "*" | "*." -> Mul
| "/" | "/." -> Div
| _ -> failwith "op_of_text"

let rec to_ocaml_heap_value = function
  Value x -> x
| ArrayExpr arr ->
    (* This arrayexpr contains only values. Turn it into a value itself. *)
    let x = Obj.new_block 0 (Array.length arr) in
      for i = 0 to Array.length arr - 1 do
        Obj.set_field x i (to_ocaml_heap_value arr.(i).e)
      done;
      x
| Cons ({e = Value h}, t) ->
    (* This list now contains only values. Turn it into a value itself. *)
    let cell = Obj.new_block 0 2 in
      Obj.set_field cell 0 h;
      Obj.set_field cell 1 (to_ocaml_heap_value t.e);
      cell
| _ -> failwith "to_ocaml_heap_value: unknown"

exception IsImplicitLet of string * Ocamli2type.t * Ocamli2type.t 

let rec finaltype_of_expression_desc env = function
  Texp_constant (Const_int x) -> Value (Obj.repr x)
| Texp_constant (Const_float x) -> Value (Obj.repr (float_of_string x))
| Texp_function {cases} ->
    let cases' = List.map (finaltype_of_case env) cases in
      Function (cases', env)
| Texp_ident (p, _, _) -> Var (Path.name p)
| Texp_construct (_, {cstr_name = "[]"}, []) -> Value (Obj.repr [])
| Texp_construct (_, {cstr_name = "::"}, [h; t]) ->
    let cons_chain =
      Cons (finaltype_of_expression env h, finaltype_of_expression env t)
    in
      if should_be_value_t' cons_chain
        then Value (to_ocaml_heap_value cons_chain)
        else cons_chain
| Texp_let (recflag, [binding], e) ->
    let (var, expr) = finaltype_of_binding env binding
    and expr' = finaltype_of_expression env e in
      if is_value expr then
        raise (IsImplicitLet (var, expr, expr'))
      else
        Let (recflag = Recursive, (var, expr), expr')
| Texp_apply
    ({exp_desc =
        Texp_ident (Path.Pdot (Path.Pident i, "@", _), _, _)},
     [(_, Some arg1); (_, Some arg2)]) when Ident.name i = "Stdlib" ->
       Append (finaltype_of_expression env arg1, finaltype_of_expression env arg2)
| Texp_apply
    ({exp_desc =
        Texp_ident (Path.Pdot (Path.Pident i, (("+" | "-" | "*" | "/") as optext), _), _, _)},
     [(_, Some arg1); (_, Some arg2)]) when Ident.name i = "Stdlib" ->
       IntOp (op_of_text optext, finaltype_of_expression env arg1, finaltype_of_expression env arg2)
| Texp_apply
    ({exp_desc =
        Texp_ident (Path.Pdot (Path.Pident i, (("+." | "-." | "*." | "/.") as optext), _), _, _)},
     [(_, Some arg1); (_, Some arg2)]) when Ident.name i = "Stdlib" ->
       FOp (op_of_text optext, finaltype_of_expression env arg1, finaltype_of_expression env arg2)
| Texp_apply
    ({exp_desc =
      Texp_ident (Path.Pdot (Path.Pdot (Path.Pident x, y, _), z, _), _, _)},
        [(_, Some arr); (_, Some index)])
      when Ident.name x = "Stdlib" && y = "Array" && z = "get" ->
        ArrayGet (finaltype_of_expression env arr, finaltype_of_expression env index)
| Texp_apply
    ({exp_desc =
      Texp_ident (Path.Pdot (Path.Pdot (Path.Pident x, y, _), z, _), _, _)},
        [(_, Some arr); (_, Some index); (_, Some newval)])
      when Ident.name x = "Stdlib" && y = "Array" && z = "set" ->
        ArraySet
          (finaltype_of_expression env arr,
           finaltype_of_expression env index,
           finaltype_of_expression env newval)
| Texp_apply (e, args) ->
    Apply (finaltype_of_expression env e,
           List.map
             (function (_, Some e') -> finaltype_of_expression env e'
                     | _ -> failwith "unknown texp_apply")
             args)
| Texp_array es ->
    let arr = Array.of_list (List.map (finaltype_of_expression env) es) in
      if should_be_value_t' (ArrayExpr arr) then
        Value (to_ocaml_heap_value (ArrayExpr arr))
      else
        ArrayExpr arr
| Texp_match (a, cases, _, _) ->
    Match (finaltype_of_expression env a, List.map (finaltype_of_case env) cases) 
| _ -> failwith "finaltype_of_expression_desc: unknown"

and finaltype_of_case env c =
  (finaltype_of_pattern env c.c_lhs,
   finaltype_of_guard env c.c_guard,
   finaltype_of_expression env c.c_rhs)

and finaltype_of_pattern env p =
  match p.pat_desc with
    Tpat_any -> PatAny
  | Tpat_construct ({txt = Lident ("[]" | "::" as cstr)}, desc, pats) ->
      PatConstr (cstr, List.map (finaltype_of_pattern env) pats)
  | Tpat_constant (Const_int i) -> PatConstant (IntConstant i)
  | Tpat_var (i, _) -> PatVar (Ident.name i)
  | _ -> failwith "finaltype_of_pattern"

and finaltype_of_guard env = function
  None -> None
| Some e -> Some (finaltype_of_expression env e)

and finaltype_of_binding env {vb_pat; vb_expr} =
  let var = match vb_pat with
    {pat_desc = Tpat_var (i, _)} -> Ident.name i
  | _ -> failwith "finaltype_of_binding: pattern not supported"
  in
    (var, finaltype_of_expression env vb_expr)

and finaltype_of_expression env exp =
  try
    {e = finaltype_of_expression_desc env exp.exp_desc;
     typ = find_type_desc exp.exp_type;
     lets = []}
  with
    IsImplicitLet (var, expr, expr') ->
      (*Printf.printf "Adding implicit let %s\n" var;*)
      {expr' with lets = (false, ref [(var, expr)]) :: expr'.lets}

let finaltype_of_typedtree {str_items} =
  match str_items with
    [{str_desc = Tstr_eval (e, _)}] ->
      finaltype_of_expression [] e
  | _ ->
      {e =
        Struct
          (List.map
            (fun x -> match x.str_desc with
              Tstr_value (recflag, [vb]) ->
                let name =
                  match vb.vb_pat.pat_desc with
                    Tpat_var (i, _) -> Ident.name i
                  | _ -> failwith "finaltype_of_typedtree2"
                in
                  {e = LetDef (recflag = Recursive, (name, finaltype_of_expression [] vb.vb_expr));
                   lets = [];
                   typ = find_type_desc vb.vb_expr.exp_type}
            | _ -> failwith "finaltype_of_typedtree")
          str_items);
       lets = [];
       typ = Types.Tnil} (* FIXME: Proper support for signature types *)

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