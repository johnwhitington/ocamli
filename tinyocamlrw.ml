open Parsetree
open Asttypes
open Tinyocaml
open Ocamliutil

exception UnknownNode of string

let rec tag_of_constructor_name_constdecls valnum blocknum str = function
  [] -> valnum, blocknum, None
| {pcd_name = {txt}; pcd_args = Pcstr_tuple args}::t when txt = str ->
    if args = []
      then valnum, blocknum, Some valnum
      else valnum, blocknum, Some blocknum
| {pcd_args = Pcstr_tuple args}::t ->
    if args = []
      then tag_of_constructor_name_constdecls (valnum + 1) blocknum str t
      else tag_of_constructor_name_constdecls valnum (blocknum + 1) str t
| {pcd_name = {txt}; pcd_args = Pcstr_record _}::t when txt = str ->
    valnum, blocknum, Some blocknum
| {pcd_args = Pcstr_record _}::t ->
    tag_of_constructor_name_constdecls valnum (blocknum + 1) str t

let rec tag_of_constructor_name_envtype valnum blocknum str (recflag, typedecls) =
  match typedecls with
    [] -> None
  | {ptype_kind = Ptype_variant constdecls}::more ->
      begin match tag_of_constructor_name_constdecls valnum blocknum str constdecls with
        _, _, Some tag -> Some tag
      | valnum', blocknum', None -> tag_of_constructor_name_envtype valnum' blocknum' str (recflag, more)
      end
  | {ptype_kind = Ptype_abstract}::more ->
      tag_of_constructor_name_envtype valnum blocknum str (recflag, more)
  | {ptype_kind = Ptype_record _}::more ->
      tag_of_constructor_name_envtype valnum blocknum str (recflag, more)
  | {ptype_kind = Ptype_open}::more -> failwith "tag_of_constructor_name_envtype: Ptype_open"

let rec tag_of_constructor_name env str =
  match env with
    [] ->
      begin match str with
        (* FIXME Not defined in stdlib + two stream ones because we don't deal with exception types in EnvType yet *)
        "Invalid_argument" | "Failure" | "None" | "Some"
      | "Stream.Error" | "Stream.Failure" | "GenHashTable.EDead" | "GenHashTable.EFalse" | "GenHashTable.ETrue" -> 0
      | _ -> 42 (* FIXME failwith (Printf.sprintf "constructor tag %s not found" str) *)
      end
  | EnvType t::more ->
      begin match tag_of_constructor_name_envtype 0 0 str t with
        None -> tag_of_constructor_name more str
      | Some tag -> tag
      end
  | _::t -> tag_of_constructor_name t str

(* For debug only *)
let tag_of_constructor_name env str =
  try tag_of_constructor_name env str with
    e ->
      Printf.printf "TAG_OF_CONSTRUCTOR failed.....!";
      print_endline (to_string_env env);
      raise e

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
| Pexp_construct ({txt = Lident "true"}, _) -> Bool true (*FIXME what if it's redefined? Also false, Nil, :: etc. *)
| Pexp_construct ({txt = Lident "false"}, _) -> Bool false
| Pexp_construct ({txt = Lident "[]"}, _) -> Nil
| Pexp_construct ({txt = Lident "::"}, Some ({pexp_desc = Pexp_tuple [e; e']})) ->
    Cons (of_real_ocaml env e, of_real_ocaml env e')
| Pexp_construct ({txt}, None) ->
    let str = string_of_longident txt in
      Constr (tag_of_constructor_name env str, str, None)
| Pexp_construct ({txt}, Some e) ->
    let str = string_of_longident txt in
      Constr (tag_of_constructor_name env str, str, Some (of_real_ocaml env e))
| Pexp_ident {txt = Lident "stdout"} -> OutChannel stdout (* FIXME As above, may be redefined *)
| Pexp_ident {txt = Lident "stderr"} -> OutChannel stderr
| Pexp_ident {txt = Lident "stdin"} -> InChannel stdin
| Pexp_ident {txt = v} -> Var (Tinyocaml.string_of_longident v)
| Pexp_ifthenelse (e, e1, Some e2) ->
    If (of_real_ocaml env e, of_real_ocaml env e1, Some (of_real_ocaml env e2))
| Pexp_ifthenelse (e, e1, None) ->
    If (of_real_ocaml env e, of_real_ocaml env e1, None)
| Pexp_fun (label, opt, pat, exp) ->
    let ocaml_exp = of_real_ocaml env exp in
      Fun (of_real_ocaml_label env label opt,
           of_real_ocaml_pattern env pat.ppat_desc,
           ocaml_exp, env)
| Pexp_function cases ->
    let cases = List.map (of_real_ocaml_case env) cases in
      Function (cases, env)
| Pexp_let (r, bindings, e') ->
    let theref = ref [] in 
    let recflag = r = Recursive in
    let bindings' = List.map (of_real_ocaml_binding (EnvBinding (recflag, theref)::env)) bindings in
      theref := bindings';
      let env' = EnvBinding (recflag, ref bindings')::env in (* FIXME [ref bindings'] or [theref] here? *)
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
| Pexp_open (_, {txt = Longident.Lident n}, e) ->
    LocalOpen (n, of_real_ocaml env e)
| Pexp_lazy e -> Lazy (of_real_ocaml env e)
| _ -> raise (UnknownNode "unknown node")

and of_real_ocaml_label env label opt =
  match label, opt with
    Nolabel, None -> NoLabel
  | Labelled l, None -> Labelled l
  | Optional l, None -> Optional (l, None)
  | Optional l, Some e -> Optional (l, Some (of_real_ocaml env e))
  | _ -> failwith "bad of_real_ocaml_label"

and of_real_ocaml_binding env {pvb_pat = {ppat_desc}; pvb_expr} =
  (of_real_ocaml_pattern env ppat_desc, of_real_ocaml env pvb_expr)

and of_real_ocaml_apps env = function
  [] -> assert false
| [x] -> of_real_ocaml env x
| h::t -> App (of_real_ocaml_apps env t, of_real_ocaml env h)

and of_real_ocaml_record_entry env = function
  ({txt}, e) -> (string_of_longident txt, ref (of_real_ocaml env e))

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
| Ppat_construct ({txt = Lident "true"}, _) -> PatBool true
| Ppat_construct ({txt = Lident "false"}, _) -> PatBool false
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
| Ppat_construct ({txt}, None) -> PatConstr (string_of_longident txt, None)
| Ppat_construct ({txt}, Some p) ->
    PatConstr (string_of_longident txt, Some (of_real_ocaml_pattern env p.ppat_desc))
| Ppat_constraint (pat, coretype) ->
    PatConstraint (of_real_ocaml_pattern env pat.ppat_desc, coretype)
| Ppat_record (items, openflag) ->
    PatRecord (openflag = Open,
               List.map (fun (n, p) -> (string_of_longident n.txt, of_real_ocaml_pattern env p.ppat_desc)) items)
| Ppat_exception p ->
    PatException (of_real_ocaml_pattern env p.ppat_desc)
| _ -> failwith "unknown pattern"

and of_real_ocaml env x =
  match x.pexp_attributes with
    [] -> of_real_ocaml_expression_desc env x.pexp_desc
  | ({txt = n}, PStr _)::_ ->
      (* Just the annotation name for now, with dummy payload. *)
      Annot (n, Struct (false, []), of_real_ocaml_expression_desc env x.pexp_desc)
  | _ -> failwith "unknown annotation"

and of_real_ocaml_primitive p =
  let n = p.pval_name.txt in
    (n, Ocamliprim.lookup_primitive ~typ:p.pval_type (List.hd p.pval_prim))

and of_real_ocaml_signature env s =
  Sig []

and of_real_ocaml_module_type env module_type =
  match module_type.pmty_desc with
    Pmty_signature s ->
      ModTypeSignature (of_real_ocaml_signature env s)
  | Pmty_ident {txt} ->
      ModTypeIdent (string_of_longident txt)
  | Pmty_functor _ -> failwith "of_real_ocaml_modtype: functor"
  | Pmty_with (mt, constraints) ->
      ModTypeWith (of_real_ocaml_module_type env mt, constraints)
  | Pmty_typeof _ -> failwith "of_real_ocaml_modtype: typeof"
  | Pmty_extension _ -> failwith "of_real_ocaml_modtype: extension"
  | Pmty_alias _ -> failwith "of_real_ocaml_modtype: alias"

and of_real_ocaml_module_expr env module_expr =
  match module_expr.pmod_desc with
    Pmod_structure s -> Struct (true, snd (of_real_ocaml_structure env [] s))
  | Pmod_constraint (module_expr, module_type) ->
      ModuleConstraint
        (of_real_ocaml_module_type env module_type,
         of_real_ocaml_module_expr env module_expr)
  | Pmod_ident {txt} ->
      ModuleIdentifier (string_of_longident txt)
  | Pmod_functor ({txt}, mt, me) ->
      let mt' =
        match mt with
        | None -> None
        | Some mt -> Some (of_real_ocaml_module_type env mt)
      in
        Functor (txt, mt', of_real_ocaml_module_expr env me)
  | Pmod_apply (me1, me2) ->
      ModuleApply
        (of_real_ocaml_module_expr env me1, of_real_ocaml_module_expr env me2)
  | Pmod_unpack _ -> failwith "of_real_ocaml_module_expr: pmod_unpack"
  | Pmod_extension _ -> failwith "of_real_ocaml_module_expr: pmod_extension"

and of_real_ocaml_module_binding env mb =
  let name = mb.pmb_name.txt in
    match of_real_ocaml_module_expr env mb.pmb_expr with
      ModuleIdentifier original ->
        (Some (ModuleBinding (name, ModuleIdentifier original)), alias_module original name env)
    | x ->
        (Some (ModuleBinding (name, x)), env)

and of_real_ocaml_open_description o =
  match o.popen_lid.txt with
    Longident.Lident x -> x
  | _ -> failwith "of_real_ocaml_open_description"

and of_real_ocaml_structure_item env = function
  (* "1" or "let x = 1 in 2" *)
  {pstr_desc = Pstr_eval (e, _)} -> (Some (of_real_ocaml env e), env)
  (* let x = 1 *)
| {pstr_desc = Pstr_value (recflag, bindings)} ->
     let theref = ref [] in
     let recflag' = recflag = Recursive in
     let bindings' = List.map (of_real_ocaml_binding (EnvBinding (recflag', theref)::env)) bindings in
       theref := bindings';
       let env' = EnvBinding (recflag', ref bindings')::env in (* FIXME [ref bindings'] or [theref]? *)
         let evalled = (LetDef (recflag', bindings')) in 
           (Some evalled, env')
  (* exception E of ... *)
| {pstr_desc = Pstr_exception {pext_name = {txt}; pext_kind = Pext_decl (t, _)}} ->
     (Some (ExceptionDef (txt, t)), env)
  (* exception E = E' *)
| {pstr_desc = Pstr_exception _} ->
     (None, env) (* FIXME *)
  (* top level attribute *)
| {pstr_desc = Pstr_attribute _} -> (None, env)
  (* external n : t = "fn" *)
| {pstr_desc = Pstr_primitive value_description} ->
    let n, primitive = of_real_ocaml_primitive value_description in
    let bindings = [(PatVar n, primitive)] in
    let env' = EnvBinding (false, ref bindings)::env in
      (Some (LetDef (false, bindings)), env')
  (* type t = A | B of int *)
| {pstr_desc = Pstr_type (recflag, typedecls)} ->
    let t = (recflag == Recursive, typedecls) in
      (*Printf.printf "Adding type %s... to env as EnvType\n" (to_string_envitem (EnvType t));*)
      (Some (TypeDef t), EnvType t::env)
  (* module M = ... *)
| {pstr_desc = Pstr_module module_binding} ->
     of_real_ocaml_module_binding env module_binding
  (* open M *)
| {pstr_desc = Pstr_open open_description} ->
     let n = of_real_ocaml_open_description open_description in
       (Some (Open n), open_module n env)
  (* module type *)
| {pstr_desc = Pstr_modtype _} ->
      (None, env)
  (* include M *)
| {pstr_desc = Pstr_include include_declaration} ->
    (Some (Include (of_real_ocaml_module_expr env include_declaration.pincl_mod)), env)
| _ -> failwith "unknown structure item"

and of_real_ocaml_structure (env : env) (acc : t list) (items : structure_item list) =
  match items with
  | [] -> env, List.rev acc
  | s::ss ->
      match of_real_ocaml_structure_item env s with
        (None, _) -> of_real_ocaml_structure env acc ss
      | (Some s, env') -> of_real_ocaml_structure env' (s::acc) ss

let of_real_ocaml env x =
  let env', str = of_real_ocaml_structure env [] x in
    (env', Struct (false, str))

let _ =
  Ocamliprim.of_real_ocaml := of_real_ocaml

(* Convert from t to an OCaml parsetree. *)
let rec to_real_ocaml_expression_desc = function
  | Control (_, x) -> to_real_ocaml_expression_desc x
  | Unit -> Pexp_construct ({txt = Longident.Lident "()"; loc = Location.none}, None)
  | Int i -> Pexp_constant (Pconst_integer (string_of_int i, None)) 
  | String s -> Pexp_constant (Pconst_string (s, None))
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
  | Fun (_, pat, e, _) ->
      Pexp_fun (Nolabel, None, to_real_ocaml_pattern pat, to_real_ocaml e)
  | App (e, e') ->
      Pexp_apply (to_real_ocaml e, [(Nolabel, to_real_ocaml e')])
  | Seq (e, e') ->
      Pexp_sequence (to_real_ocaml e, to_real_ocaml e')
  | e ->
      Printf.printf "Unknown thing in to_real_ocaml_expression_desc: %s\n"
      (to_string e);
      failwith "fix to_real_ocaml_expression_desc"

and to_real_ocaml_pattern = function
  PatInt i ->
    {ppat_desc = Ppat_constant (Pconst_integer (string_of_int i, None));
     ppat_loc = Location.none;
     ppat_attributes = []}
| PatAny ->
    {ppat_desc = Ppat_any;
     ppat_loc = Location.none;
     ppat_attributes = []}
| PatVar v ->
    {ppat_desc = Ppat_var {txt = v; loc = Location.none};
     ppat_loc = Location.none;
     ppat_attributes = []}
| x -> failwith (Printf.sprintf "to_real_ocaml_pattern %s" (Tinyocaml.to_string_pat x))

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

let to_real_ocaml = function
  | Struct (_, xs) ->
      Printf.printf "Processing a struct of %i items\n" (List.length xs);
      List.map
        (function
           LetDef (recflag, bindings) ->
             {pstr_desc =
                Pstr_value
                  ((if recflag then Recursive else Nonrecursive),
                   List.map to_real_ocaml_binding bindings);
              pstr_loc = Location.none}
         | x -> 
           {pstr_desc = Pstr_eval (to_real_ocaml x, []);
            pstr_loc = Location.none})
        xs
  | x ->
      Printf.printf "Processing something else at top level\n";
      [{pstr_desc = Pstr_eval (to_real_ocaml x, []);
        pstr_loc = Location.none}]

(* For debug, take something like:
  *
  * type t = A | B of int
  *
  * let _ = B 5
  *
  *
  * and return a Tinyocaml.t representation of expression "B 5" and the Tinyocaml.env including 't' *)
let extract_from_let = function
  LetDef (_, [(_, e)]) -> e
| e -> e

let of_string s =
  match of_real_ocaml [] (ast s) with
    env, Struct (_, (_::_ as l)) ->
      (env, extract_from_let (List.hd (List.rev l)))
  | _ -> failwith "Tinyocaml.of_string"

