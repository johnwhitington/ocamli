open Typedtree
open Type
open Types

let op_of_text = function
  "+" | "+." -> Add
| "-" | "-." -> Sub
| "*" | "*." -> Mul
| "/" | "/." -> Div
| _ -> failwith "op_of_text"

let compop_of_text = function
  "<" -> LT
| ">" -> GT
| "<=" -> LTE
| ">=" -> GTE
| "<>" -> NEQ
| "=" -> EQ
| _ -> failwith "compop_of_text"

let boolop_of_text = function
  "&&" -> AND
| "||" -> OR
| _ -> failwith "boolop_of_text"

let source = ref 0

(* Follow any Tlinks left from typechecking, to make pattern matching on types easier. *)
let rec remove_links type_expr =
  match type_expr.desc with
    Tlink x -> remove_links x
  | Tvar _ | Tnil | Tvariant _ | Tunivar _ -> type_expr
  | x ->
      {type_expr with desc =
         begin match x with
           Tarrow (arg_label, a, b, commutable) -> Tarrow (arg_label, remove_links a, remove_links b, commutable)
          | Ttuple ts -> Ttuple (List.map remove_links ts)
          | Tconstr (path, ts, abbrev_memo) -> Tconstr (path, List.map remove_links ts, abbrev_memo)
          | Tobject (t, ({contents = None} as r)) -> Tobject (remove_links t, r)
          | Tobject (t, ({contents = Some (p, ts)} as r)) ->
              r := Some (p, List.map remove_links ts);
              Tobject (remove_links t, r)
          | Tfield (s, field_kind, a, b) -> Tfield (s, field_kind, remove_links a, remove_links b)
          | Tsubst t -> Tsubst (remove_links t)
          | Tpoly (t, ts) -> Tpoly (remove_links t, List.map remove_links ts)
          | Tpackage (path, idents, ts) -> Tpackage (path, idents, List.map remove_links ts)
          | x -> x
         end}

let rec debug_type type_expr =
  {type_expr with desc =
     begin match type_expr.desc with
       Tvar None -> Tvar (Some (source := !source + 1; "DEBUG-TVARNONE " ^ string_of_int !source))
     | Tarrow (label, a, b, commutable) -> Tarrow (label, debug_type a, debug_type b, commutable)
     | Ttuple ts -> Ttuple (List.map debug_type ts)
     | Tconstr (path, ts, a) -> Tconstr (path, List.map debug_type ts, a)
     | Tlink t -> failwith "debug_type: should be no links after reading"
     | x -> x
     end}

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

exception IsImplicitLet of string * Type.t * Type.t 

let rec finaltype_of_expression_desc env = function
  Texp_constant (Const_int x) -> Value (Obj.repr x)
| Texp_constant (Const_float x) -> Value (Obj.repr (float_of_string x))
| Texp_function {cases} ->
    let cases' = List.map (finaltype_of_case env) cases in
      Function (cases', env)
| Texp_ident (p, _, _) -> Var (Path.name p)
| Texp_construct (_, {cstr_name = "[]"}, []) -> Value (Obj.repr [])
| Texp_construct (_, {cstr_name = "true"}, []) -> Value (Obj.repr true)
| Texp_construct (_, {cstr_name = "false"}, []) -> Value (Obj.repr false)
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
        Texp_ident (Path.Pdot (Path.Pident i, "@"), _, _)},
     [(_, Some arg1); (_, Some arg2)]) when Ident.name i = "Stdlib" ->
       Append (finaltype_of_expression env arg1, finaltype_of_expression env arg2)
| Texp_apply
    ({exp_desc =
        Texp_ident (Path.Pdot (Path.Pident i, (("+" | "-" | "*" | "/") as optext)), _, _)},
     [(_, Some arg1); (_, Some arg2)]) when Ident.name i = "Stdlib" ->
       IntOp (op_of_text optext, finaltype_of_expression env arg1, finaltype_of_expression env arg2)
| Texp_apply
    ({exp_desc =
        Texp_ident (Path.Pdot (Path.Pident i, (("+." | "-." | "*." | "/.") as optext)), _, _)},
     [(_, Some arg1); (_, Some arg2)]) when Ident.name i = "Stdlib" ->
       FOp (op_of_text optext, finaltype_of_expression env arg1, finaltype_of_expression env arg2)
| Texp_apply
    ({exp_desc =
        Texp_ident (Path.Pdot (Path.Pident i, (("<" | ">" | "=" | "<>" | "<=" | ">=") as optext)), _, _)},
     [(_, Some arg1); (_, Some arg2)]) when Ident.name i = "Stdlib" ->
       Compare (compop_of_text optext, finaltype_of_expression env arg1, finaltype_of_expression env arg2)
| Texp_apply
    ({exp_desc =
        Texp_ident (Path.Pdot (Path.Pident i, (("&&" | "||") as optext)), _, _)},
     [(_, Some arg1); (_, Some arg2)]) when Ident.name i = "Stdlib" ->
       BoolOp (boolop_of_text optext, finaltype_of_expression env arg1, finaltype_of_expression env arg2)
| Texp_apply
    ({exp_desc =
      Texp_ident (Path.Pdot (Path.Pdot (Path.Pident x, y), z), _, _)},
        [(_, Some arr); (_, Some index)])
      when Ident.name x = "Stdlib" && y = "Array" && z = "get" ->
        ArrayGet (finaltype_of_expression env arr, finaltype_of_expression env index)
| Texp_apply
    ({exp_desc =
      Texp_ident (Path.Pdot (Path.Pdot (Path.Pident x, y), z), _, _)},
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
| Texp_match (a, cases, _) ->
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
     typ = debug_type (remove_links exp.exp_type);
     lets = [];
     peek = None;
     printas = None}
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
                   typ = debug_type (remove_links vb.vb_expr.exp_type);
                   peek = None;
                   printas = None}
            | _ -> failwith "finaltype_of_typedtree")
          str_items);
       lets = [];
       typ = {level = 0; id = 0; scope = 0; desc = Types.Tnil};
       peek = None;
       printas = None} (* FIXME: Proper support for signature types *)

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

let read x =
  finaltype_of_typedtree (typedtree_of_string x)

