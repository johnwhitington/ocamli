open Parsetree
open Asttypes
open Tinyocaml

exception UnknownNode of string

let bound_in_environment_item (_, bindings) =
  List.flatten (List.map (fun (p, _) -> bound_in_pattern p) bindings)

let bound_in_environment env =
  List.flatten (List.map bound_in_environment_item env)

(* List the identifiers used in an expression which are not defined in it. *)
let rec free (bound : string list) (expr : t) =
  match expr with
  | Var s -> if List.mem s bound then [] else [s]
  (* Things which can bind names (and may contain subexpressions too) *)
  | Let (recflag, bindings, e) ->
      free_in_bindings bound recflag bindings e
  | LetDef (recflag, bindings) ->
      free_in_bindings bound recflag bindings Unit
  | Fun (pattern, e, _) ->
      free (bound_in_pattern pattern @ bound) e
  | Function (cases, _) ->
      free_in_cases bound cases
  | For (name, e, _, e', e'', _) ->
      free bound e @ free bound e' @ free (name::bound) e''
  | Match (e, cases) ->
      free bound e @ free_in_cases bound cases
  (* Other things which contain subexpressions *)
  | Record items ->
      List.fold_left ( @ ) []
        (List.map (free bound) (List.map (fun (_, {contents}) -> contents) items))
  | Struct (_, es)
  | Tuple es
  | Sig es ->
      List.fold_left ( @ ) [] (List.map (free bound) es)
  | Array es ->
      Array.fold_left ( @ ) [] (Array.map (free bound) es)
  | Raise (_, Some e)
  | Assert e
  | Field (e, _)
  | TryWith (e, _)
  | Control (_, e) ->
      free bound e
  | Op (_, e, e')
  | Cmp (_, e, e')
  | Append (e, e')
  | While (e, e', _, _)
  | SetField (e, _, e')
  | App (e, e')
  | Seq (e, e') ->
      free bound e @ free bound e'
  | If (e, e', e'') ->
      free bound e @ free bound e' @
      (match e'' with None -> [] | Some e'' -> free bound e'')
  (* All others *)
  | x -> [] (* FIXME add cons, constr, any others. *)

(* A variable is free in a case if it is free in the guard or rhs *)
and free_in_case bound (pat, guard, rhs) =
  let bound = bound_in_pattern pat @ bound in
      free bound rhs @ (match guard with None -> [] | Some g -> free bound g)

and free_in_cases bound cases =
  List.flatten (List.map (free_in_case bound) cases)

and free_in_binding bound (pat, t) =
  free bound t

and free_in_bindings bound recflag bindings e =
  let bound' =
    List.flatten (List.map (fun (p, _) -> bound_in_pattern p) bindings)
  in
     List.flatten
       (List.map (free_in_binding (if recflag then bound' else bound)) bindings)
   @ free bound' e

let free = free []

(* Given a list of variables free in some code, and the current environment,
produce a new environment containing just those ones which are free.
Duplicates and the order are retained *)
let any_var_in_bindings free ((_, bindings) as envitem) =
  if
    List.exists
      (fun x -> List.mem x free)
      (List.flatten (List.map (fun (p, _) -> bound_in_pattern p) bindings))
  then
    Some envitem
  else
    None

let prune_environment (free : string list) (env : env) : env =
  Ocamliutil.option_map (any_var_in_bindings free) env

(* Convert from a parsetree to a t, assuming we can *)
let rec of_real_ocaml_expression_desc env = function
  Pexp_constant (Pconst_integer (s, None)) -> Int (int_of_string s)
| Pexp_constant (Pconst_integer (s, Some 'l')) -> Int32 (Int32.of_string s)
| Pexp_constant (Pconst_integer (s, Some 'L')) -> Int64 (Int64.of_string s)
| Pexp_constant (Pconst_integer (s, Some 'n')) -> NativeInt (Nativeint.of_string s)
| Pexp_constant (Pconst_char c) -> Char c
| Pexp_constant (Pconst_string (s, None)) -> String s
| Pexp_constant (Pconst_float (s, None)) -> Float (float_of_string s)
| Pexp_construct ({txt = Lident "()"}, _) -> Unit
| Pexp_construct ({txt = Lident "true"}, _) -> Bool true
| Pexp_construct ({txt = Lident "false"}, _) -> Bool false
| Pexp_construct ({txt = Lident "[]"}, _) -> Nil
| Pexp_construct ({txt = Lident "::"}, Some ({pexp_desc = Pexp_tuple [e; e']})) ->
    Cons (of_real_ocaml env e, of_real_ocaml env e')
| Pexp_construct ({txt = Lident x}, None) ->
    Constr (x, None)
| Pexp_construct ({txt = Lident x}, Some e) ->
    Constr (x, Some (of_real_ocaml env e))
| Pexp_ident {txt = Lident "stdout"} -> OutChannel stdout
| Pexp_ident {txt = Lident "stderr"} -> OutChannel stderr
| Pexp_ident {txt = Lident "stdin"} -> InChannel stdin
| Pexp_ident {txt = v} -> Var (Tinyocaml.string_of_longident v)
| Pexp_ifthenelse (e, e1, Some e2) ->
    If (of_real_ocaml env e, of_real_ocaml env e1, Some (of_real_ocaml env e2))
| Pexp_ifthenelse (e, e1, None) ->
    If (of_real_ocaml env e, of_real_ocaml env e1, None)
| Pexp_fun (Nolabel, None, pat, exp) ->
    let ocaml_exp = of_real_ocaml env exp in
    (*let bound = bound_in_environment env in*)
      (*Printf.printf "%i variables bound in environment\n" (List.length env);*)
      let free_in_exp = free ocaml_exp in
        (*Printf.printf "%i variable free in function\n" (List.length * free_in_exp);*)
      let environment = prune_environment free_in_exp env in
        (*Printf.printf "Built function environment of %i bindings\n" * (List.length environment);*)
        Fun (of_real_ocaml_pattern env pat.ppat_desc, ocaml_exp, environment)
| Pexp_fun _ -> failwith "unknown node fun"
| Pexp_function cases ->
    let cases = List.map (of_real_ocaml_case env) cases in
      (*let bound = bound_in_environment env in*)
      let environment = prune_environment (free (Function (cases, []))) env in 
        Function (cases, environment)
| Pexp_let (r, bindings, e') ->
    let recflag = r = Recursive
    and bindings' = List.map (of_real_ocaml_binding env) bindings in
      let env' = (recflag, bindings')::env in
        Let (recflag, bindings', of_real_ocaml env' e')
| Pexp_apply
    ({pexp_desc = Pexp_ident {txt = Longident.Lident "raise"}},
     [(Nolabel, {pexp_desc = Pexp_construct ({txt = Longident.Lident s}, payload)})]) ->
         begin match payload with
           None -> Raise (s, None)
         | Some x -> Raise (s, Some (of_real_ocaml env x))
         end
| Pexp_apply (* 2 operands *)
    ({pexp_desc = Pexp_ident {txt = Longident.Lident f}},
     [(Nolabel, l); (Nolabel, r)]) ->
       let e = of_real_ocaml env l in
       let e' = of_real_ocaml env r in
         begin match f with
           "&&" -> And (e, e')
         | "||" -> Or (e, e')
         | "@" -> Append (e, e')
         | ("*" | "+" | "-" | "/") as op  -> Op (op_of_string op, e, e')
         | ("=" | ">" | "<" | "<=" | ">=" | "<>") as cmp ->
             Cmp (cmp_of_string cmp , e, e')

         | _ -> App (App (Var f, e), e') 
         end
| Pexp_apply (e, [(Nolabel, e')]) -> (* one operand *)
    App (of_real_ocaml env e, of_real_ocaml env e')
| Pexp_apply (e, apps) -> (* more than two operands *)
    of_real_ocaml_apps env (List.rev (e::List.map snd apps))
| Pexp_sequence (e, e') ->
    Seq (of_real_ocaml env e, of_real_ocaml env e')
| Pexp_while (e, e') ->
    While (of_real_ocaml env e, of_real_ocaml env e', of_real_ocaml env e, of_real_ocaml env e')
| Pexp_for ({ppat_desc = Ppat_var {txt}}, e, e', flag, e'') ->
    let convert_flag = function Upto -> UpTo | Downto -> DownTo in
      For
        (txt, of_real_ocaml env e, convert_flag flag,
         of_real_ocaml env e', of_real_ocaml env e'', of_real_ocaml env e'')
| Pexp_record (items, _) ->
    Record (List.map (of_real_ocaml_record_entry env) items)
| Pexp_field (e, {txt = Longident.Lident n}) ->
    Field (of_real_ocaml env e, n)
| Pexp_setfield (e, {txt = Longident.Lident n}, e') ->
    SetField (of_real_ocaml env e, n, of_real_ocaml env e')
| Pexp_try (e, cases) ->
    TryWith (of_real_ocaml env e, List.map (of_real_ocaml_case env) cases)
| Pexp_tuple xs ->
    Tuple (List.map (of_real_ocaml env) xs)
| Pexp_array xs ->
    Array (Array.of_list (List.map (of_real_ocaml env) xs))
| Pexp_match (e, cases) ->
    Match (of_real_ocaml env e, List.map (of_real_ocaml_case env) cases)
| Pexp_assert e ->
    Assert (of_real_ocaml env e)
| Pexp_newtype (_, e) -> of_real_ocaml env e
| Pexp_constraint (e, _) -> of_real_ocaml env e
| _ -> raise (UnknownNode "unknown node")

and of_real_ocaml_binding env {pvb_pat = {ppat_desc}; pvb_expr} =
  (of_real_ocaml_pattern env ppat_desc, of_real_ocaml env pvb_expr)

and of_real_ocaml_apps env = function
  [] -> assert false
| [x] -> of_real_ocaml env x
| h::t -> App (of_real_ocaml_apps env t, of_real_ocaml env h)

and of_real_ocaml_record_entry env = function
  ({txt = Longident.Lident n}, e) -> (n, ref (of_real_ocaml env e))
| _ -> raise (UnknownNode "unknown record entry type")

and of_real_ocaml_case env {pc_lhs; pc_guard; pc_rhs} =
  (of_real_ocaml_pattern env pc_lhs.ppat_desc,
   begin match pc_guard with None -> None | Some x -> Some (of_real_ocaml env x) end,
   of_real_ocaml env pc_rhs)

and of_real_ocaml_pattern env = function
  Ppat_var {txt} -> PatVar txt
| Ppat_constant (Pconst_integer (s, None)) -> PatInt (int_of_string s)
| Ppat_constant (Pconst_integer (s, Some 'l')) -> PatInt32 (Int32.of_string s)
| Ppat_constant (Pconst_integer (s, Some 'L')) -> PatInt64 (Int64.of_string s)
| Ppat_constant (Pconst_integer (s, Some 'n')) -> PatNativeInt (Nativeint.of_string s)
| Ppat_constant (Pconst_char c) -> PatChar c
| Ppat_interval (Pconst_char c, Pconst_char c') -> PatCharRange (c, c')
| Ppat_constant (Pconst_string (s, _)) -> PatString s
| Ppat_any -> PatAny
| Ppat_tuple patterns ->
    PatTuple
      (List.map (of_real_ocaml_pattern env) (List.map (fun x -> x.ppat_desc) patterns))
| Ppat_array patterns ->
    PatArray
      (Array.of_list (List.map (of_real_ocaml_pattern env) (List.map (fun x -> x.ppat_desc) patterns)))
| Ppat_construct ({txt = Lident "[]"}, _) -> PatNil
| Ppat_construct ({txt = Lident "()"}, _) -> PatUnit
| Ppat_construct ({txt = Lident "::"}, Some ({ppat_desc = Ppat_tuple [a; b]})) ->
    PatCons (of_real_ocaml_pattern env a.ppat_desc, of_real_ocaml_pattern env b.ppat_desc)
| Ppat_alias (pattern, {txt}) ->
    PatAlias (txt, of_real_ocaml_pattern env pattern.ppat_desc)
| Ppat_or (p, p') ->
    PatOr
      (of_real_ocaml_pattern env p.ppat_desc,
       of_real_ocaml_pattern env p'.ppat_desc)
| Ppat_construct ({txt = Lident x}, None) -> PatConstr (x, None)
| Ppat_construct ({txt = Lident x}, Some p) ->
    PatConstr (x, Some (of_real_ocaml_pattern env p.ppat_desc))
| Ppat_constraint (pat, coretype) ->
    PatConstraint (of_real_ocaml_pattern env pat.ppat_desc, coretype)
| _ -> failwith "unknown pattern"

and of_real_ocaml env x = of_real_ocaml_expression_desc env x.pexp_desc

and of_real_ocaml_primitive p =
  let n = p.pval_name.txt in
    (n, Ocamliprim.lookup_primitive (List.hd p.pval_prim))

and of_real_ocaml_structure env s =
  (* FIXME env *)
  let items =
    List.map (of_real_ocaml_structure_item env) s
  in
    let final =
      Ocamliutil.option_map (fun x -> x) (List.map fst items)
    in
      Struct (true, final)

and of_real_ocaml_signature env s =
  Sig []

and of_real_ocaml_module_type env module_type =
  match module_type.pmty_desc with
    Pmty_signature s ->
      ModTypeSignature (of_real_ocaml_signature env s)
  | _ -> failwith "of_real_ocaml_module_type"

and of_real_ocaml_module_expr env module_expr =
  match module_expr.pmod_desc with
    Pmod_structure s -> of_real_ocaml_structure env s
  | Pmod_constraint (module_expr, module_type) ->
      ModuleConstraint
        (of_real_ocaml_module_type env module_type,
         of_real_ocaml_module_expr env module_expr)
  | _ -> failwith "of_real_ocaml_module_expr"

and of_real_ocaml_module_binding env mb =
  let name = mb.pmb_name.txt in
    ModuleBinding (name, of_real_ocaml_module_expr env mb.pmb_expr)

and of_real_ocaml_open_description o =
  match o.popen_lid.txt with
    Longident.Lident x -> x
  | _ -> failwith "of_real_ocaml_open_description"

and of_real_ocaml_structure_item_inner env = function
  (* "1" or "let x = 1 in 2" *)
  {pstr_desc = Pstr_eval (e, _)} -> (Some (of_real_ocaml env e), env)
  (* let x = 1 *)
| {pstr_desc = Pstr_value (recflag, bindings)} ->
     let recflag' = recflag = Recursive
     and bindings' = List.map (of_real_ocaml_binding env) bindings in
       let env' = (recflag', bindings')::env in
         (Some (LetDef (recflag', bindings')), env')
  (* exception E of ... *)
| {pstr_desc = Pstr_exception {pext_name = {txt}; pext_kind = Pext_decl (t, _)}} ->
     (Some (ExceptionDef (txt, t)), env)
| {pstr_desc = Pstr_attribute _} -> (None, env)
  (* external n : t = "fn" *)
| {pstr_desc = Pstr_primitive value_description} ->
    let n, primitive = of_real_ocaml_primitive value_description in
    let bindings = [(PatVar n, primitive)] in
    let env' = (false, bindings)::env in
      (Some (LetDef (false, bindings)), env')
  (* type t = A | B of int *)
| {pstr_desc = Pstr_type (recflag, typedecls)} ->
     (Some (TypeDef (recflag == Recursive, typedecls)), env)
  (* module M = ... *)
| {pstr_desc = Pstr_module module_binding} ->
     (Some (of_real_ocaml_module_binding env module_binding), env)
  (* open M *)
| {pstr_desc = Pstr_open open_description} ->
     (Some (Open (of_real_ocaml_open_description open_description)), env)
| _ -> failwith "unknown structure item"

(* Get the structure item and then evaluate its right hand side, effectively
doing module initilization at parse time *)
and of_real_ocaml_structure_item env e =
  match of_real_ocaml_structure_item_inner env e with
    (None, env) -> (None, env)
  | (Some e, env) -> (Some e, env) (* (Some (Eval.eval false env e), env) *)

let rec of_real_ocaml env acc = function
  | [] -> List.rev acc
  | s::ss ->
      match of_real_ocaml_structure_item env s with
        (None, _) -> of_real_ocaml env acc ss
      | (Some s, env') -> of_real_ocaml env' (s::acc) ss

let of_real_ocaml x =
  Struct (false, of_real_ocaml [] [] x)

(* Convert from t to an OCaml parsetree. *)
let rec to_real_ocaml_expression_desc = function
  | Control (_, x) -> to_real_ocaml_expression_desc x
  | Unit -> Pexp_construct ({txt = Longident.Lident "()"; loc = Location.none}, None)
  | Int i -> Pexp_constant (Pconst_integer (string_of_int i, None)) 
  | Bool b ->
      Pexp_construct
        ({txt = Longident.Lident (string_of_bool b); loc = Location.none},
          None)
  | Var v ->
      Pexp_ident {txt = Longident.Lident v; loc = Location.none}
  | Op (op, l, r) -> to_real_ocaml_apply l r (string_of_op op)
  | And (l, r) -> to_real_ocaml_apply l r "&&"
  | Or (l, r) -> to_real_ocaml_apply l r "||"
  | Cmp (cmp, l, r) -> to_real_ocaml_apply l r (string_of_cmp cmp)
  | If (e, e1, None) ->
      Pexp_ifthenelse (to_real_ocaml e, to_real_ocaml e1, None)
  | If (e, e1, Some e2) ->
      Pexp_ifthenelse (to_real_ocaml e, to_real_ocaml e1, Some (to_real_ocaml e2))
  | Let (flag, bindings, e) -> to_real_ocaml_let flag bindings e
  | Fun (pat, e, _) ->
      Pexp_fun (Nolabel, None, to_real_ocaml_pattern pat, to_real_ocaml e)
  | App (e, e') ->
      Pexp_apply (to_real_ocaml e, [(Nolabel, to_real_ocaml e')])
  | Seq (e, e') ->
      Pexp_sequence (to_real_ocaml e, to_real_ocaml e')
  | Struct (_, [x]) -> to_real_ocaml_expression_desc x (* FIXME *)
  | e ->
      Printf.printf "Unknown thing in to_real_ocaml_expression_desc: %s\n"
      (to_string e);
      failwith "fix to_real_ocaml_expression_desc"

and to_real_ocaml_pattern = function
  PatInt i ->
    {ppat_desc = Ppat_constant (Pconst_integer (string_of_int i, None));
     ppat_loc = Location.none;
     ppat_attributes = []}
| _ -> failwith "to_real_ocaml_pattern"

and to_real_ocaml_binding (pat, t) =
  {pvb_pat = to_real_ocaml_pattern pat;
   pvb_expr = to_real_ocaml t;
   pvb_attributes = [];
   pvb_loc = Location.none}

and to_real_ocaml_let r bs e =
  let bindings = List.map to_real_ocaml_binding bs in
    Pexp_let
      ((if r then Recursive else Nonrecursive), bindings, to_real_ocaml e)

and to_real_ocaml_apply l r n =
  let exprs =
    [(Nolabel, to_real_ocaml l); (Nolabel, to_real_ocaml r)] in
  let expr =
    Ocamliutil.with_desc
      (Pexp_ident
         {txt = Longident.Lident n; loc = Location.none})
  in
    Pexp_apply (expr, exprs)

and to_real_ocaml x =
  Ocamliutil.with_desc (to_real_ocaml_expression_desc x)

(* Just a single structure item for now *)
let to_real_ocaml x =
  [{pstr_desc = Pstr_eval (to_real_ocaml x, []);
    pstr_loc = Location.none}]
