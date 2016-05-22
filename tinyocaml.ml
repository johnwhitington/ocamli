open Parsetree
open Asttypes

type op = Add | Sub | Mul | Div

type cmp = LT | EQ | GT | EQLT | EQGT | NEQ

type control = Underline | Bold

type forkind = UpTo | DownTo

type pattern =
  PatAny
| PatVar of string
| PatInt of int
| PatUnit
| PatTuple of pattern list
| PatNil
| PatCons of pattern * pattern
| PatAlias of string * pattern

and case = pattern * t option * t (* pattern, guard, rhs *)

and expatmatch = string * t (* for now *)

and binding = pattern * t

and t =
  Unit                        (* () *)
| Int of int                  (* 1 *)
| Bool of bool                (* false *)
| Float of float              (* 1.0 *)
| String of string            (* "foo" *)
| OutChannel of out_channel   (* e.g stdout, stderr *)
| InChannel of in_channel     (* e.g stdin *)
| Var of string               (* x *)
| Record of (string * t ref) list  (* Records. *)
| Op of (op * t * t)          (* + - / * *)
| And of (t * t)              (* && *)
| Or of (t * t)               (* || *)
| Cmp of (cmp * t * t)        (* < > <> = <= >= *)
| If of (t * t * t)           (* if e then e1 else e2 *)
| Let of (bool * binding list * t)    (* let x = e [and ...] in e' *)
| LetDef of (bool * binding list)     (* let x = e [and ...] *)
| Fun of (string * t)         (* fun x -> e *)
| Function of case list     
| App of (t * t)              (* e e' *)
| Seq of (t * t)              (* e; e *)
| While of (t * t * t * t)    (* while e do e' done (e, e', copy_of_e copy_of_e') *)
| For of (string * t * forkind * t * t * t) (* for e [UpTo | DownTo] e' do e'' done (copy of e'') *)
| Field of (t * string)       (* e.y *)
| SetField of (t * string * t)(* e.y <- e' *)
| Raise of (string * t option)(* raise e *)
| Match of (t * case list)     (* match e with ... *)
| TryWith of (t * expatmatch) (* try e with ... *)
| ExceptionDef of (string * Parsetree.constructor_arguments) (* Exception definition. *)
| Control of (control * t)    (* Control string for prettyprinting *)
| CallBuiltIn of (string * t list * (t list -> t)) (* A built-in. Recieves args, returns result *)
| Struct of (string * t list)   (* Module implementation. *)
| Sig of t list               (* Module signature. *)
| Cons of (t * t)             (* List *)
| Nil                         (* [] *)
| Append of (t * t)           (* @ *)
| Tuple of t list             (* (1, 2) *)
| Assert of t                 (* assert *)

let string_of_op = function
  Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

let string_of_cmp = function
  LT -> "<" | EQ -> "=" | GT -> ">" | EQLT -> "<=" | EQGT -> ">=" | NEQ -> "<>"

let op_of_string = function
  "+" -> Add | "-" -> Sub | "*" -> Mul | "/" -> Div
| _ -> failwith "op_of_string"

let cmp_of_string = function
  "<" -> LT | "=" -> EQ | ">" -> GT | "<=" -> EQLT | ">=" -> EQGT | "<>" -> NEQ
| _ -> failwith "cmp_of_string"

let string_of_coretype t =
  let f = Format.str_formatter in
    Pprintast.core_type f t;
    Format.flush_str_formatter ()

let string_of_constructor_arg = function
  Pcstr_tuple coretypes ->
    string_of_coretype
      {ptyp_desc = Ptyp_tuple coretypes;
       ptyp_loc = Location.none; 
       ptyp_attributes = []}
| Pcstr_record _ -> "record"

let rec to_string = function
  Unit -> "Unit"
| Assert e -> Printf.sprintf "Assert %s" (to_string e)
| Int i -> Printf.sprintf "Int %i" i
| Bool b -> Printf.sprintf "Bool %b" b
| Float f -> Printf.sprintf "Float %f" f
| String s -> Printf.sprintf "String %s" s
| OutChannel o -> Printf.sprintf "OutChannel"
| InChannel i -> Printf.sprintf "InChannel"
| Var s -> Printf.sprintf "Var %s" s
| Record l -> to_string_record l
| Op (op, l, r) ->
    Printf.sprintf "Op (%s, %s, %s)" (to_string_op op) (to_string l) (to_string r)
| And (a, b) ->
    Printf.sprintf "And (%s, %s)" (to_string a) (to_string b)
| Or (a, b) ->
    Printf.sprintf "Or (%s, %s)" (to_string a) (to_string b)
| Cmp (cmp, l, r) -> 
    Printf.sprintf "Cmp (%s, %s, %s)" (to_string_cmp cmp) (to_string l) (to_string r)
| If (a, b, c) ->
    Printf.sprintf "If (%s, %s, %s)" (to_string a) (to_string b) (to_string c)
| Let (recflag, bindings, e') ->
    Printf.sprintf "%s (%s, %s)"
      (if recflag then "LetRec" else "Let") (to_string_bindings bindings) (to_string e')
| LetDef (recflag, bindings) ->
    Printf.sprintf "%s (%s)"
      (if recflag then "LetDefRec" else "LetDef") (to_string_bindings bindings)
| Fun (fname, fexp) ->
    Printf.sprintf "Fun (%s, %s)" fname (to_string fexp)
| App (e, e') ->
    Printf.sprintf "App (%s, %s)" (to_string e) (to_string e')
| Seq (e, e') ->
    Printf.sprintf "Seq (%s, %s)" (to_string e) (to_string e')
| While (e, e', copy_e, copy_e') ->
    Printf.sprintf "While (%s, %s, %s, %s)"
      (to_string e) (to_string e') (to_string copy_e) (to_string copy_e')
| For (var, s, forkind, e, expr, copy_expr) ->
    Printf.sprintf "For (%s, %s, %s, %s, %s, %s)"
      var (to_string s) (to_string_forkind forkind) (to_string e) (to_string expr) (to_string copy_expr)
| Field (e, y) ->
    Printf.sprintf "Field (%s, %s)" (to_string e) y
| SetField (e, y, e') ->
    Printf.sprintf "SetField (%s, %s, %s)" (to_string e) y (to_string e')
| Raise (n, payload) ->
    Printf.sprintf
      "Raise %s (%s)"
      n (match payload with None -> "" | Some x -> to_string x)
| ExceptionDef (e, args) ->
    Printf.sprintf "Exception (%s, Some %s)" e (string_of_constructor_arg args)
| TryWith (t, pat) ->
    Printf.sprintf "TryWith (%s, %s)" (to_string t) (to_string_expatmatch pat)
| Control (c, t) ->
    Printf.sprintf "Control (%s, %s)" (to_string_control c) (to_string t)
| CallBuiltIn (name, _, _) ->
    Printf.sprintf "CallBuiltIn %s" name
| Struct l ->
    to_string_struct l
| Cons (e, e') ->
    Printf.sprintf "Cons (%s, %s)" (to_string e) (to_string e')
| Nil -> "[]"
| Append (e, e') ->
    Printf.sprintf "Append (%s, %s)" (to_string e) (to_string e')
| Tuple xs ->
    Printf.sprintf
      "Tuple (%s)"
      (List.fold_left ( ^ ) "" (List.map (fun x -> to_string x ^ ", ") xs))
| Function patmatch ->
    Printf.sprintf "Function %s" (to_string_patmatch patmatch)
| Match (e, patmatch) ->
    Printf.sprintf
      "Match (%s, %s)" (to_string e) (to_string_patmatch patmatch)

and to_string_bindings bs =
  List.fold_left ( ^ ) "" (List.map to_string_binding bs)

and to_string_binding (pat, e) =
  Printf.sprintf "%s = %s\n" (to_string_pat pat) (to_string e)

and to_string_pat = function
  PatAny -> "_"
| PatVar v -> v
| PatInt i -> string_of_int i
| PatUnit -> "()"
| PatTuple _ -> "PatTuple"
| PatNil -> "[]"
| PatCons _ -> "PatCons"
| PatAlias _ -> "PatAlias"

and to_string_patmatch xs =
  List.fold_left ( ^ ) "" (List.map (fun x -> to_string_case x ^ ", ") xs)

and to_string_guard = function
  None -> "None"
| Some g -> Printf.sprintf "Some (%s)" (to_string g)

and to_string_case (pat, guard, rhs) =
  Printf.sprintf "(%s, %s, %s)" (to_string_pat pat) (to_string_guard guard) (to_string rhs)

and to_string_expatmatch (s, t) =
  Printf.sprintf "(%s, %s)" s (to_string t)

and to_string_control = function
  Underline -> "Underline"
| Bold -> "Bold"

and to_string_op = function
  Add -> "Add" | Sub -> "Sub" | Mul -> "Mul" | Div -> "Div"

and to_string_cmp = function
  LT -> "LT" | EQ -> "EQ" | GT -> "GT" | EQLT -> "EQLT" | EQGT -> "EQGT" | NEQ -> "NEQ"

and to_string_forkind = function UpTo -> "UpTo" | DownTo -> "DownTo" 

and to_string_record l =
  "Record [" ^
  List.fold_left ( ^ ) ""
    (List.map (fun (n, t) -> Printf.sprintf "(%s, %s); " n (to_string !t)) l) ^
  "]"

and to_string_struct (name, l) =
  Printf.sprintf "Struct %s = [" name ^
  List.fold_left ( ^ ) "" (List.map (fun x -> to_string x ^ "\n") l) ^
  "]"

exception UnknownNode of string

let rec dots_between = function
  [] -> ""
| [h] -> h
| h::t -> h ^ "." ^ dots_between t

let string_of_longident l =
  dots_between (Longident.flatten l)

(* Convert from a parsetree to a t, assuming we can *)
let rec of_real_ocaml_expression_desc = function
  Pexp_constant (Pconst_integer (s, None)) -> Int (int_of_string s)
| Pexp_constant (Pconst_string (s, None)) -> String s
| Pexp_constant (Pconst_float (s, None)) -> Float (float_of_string s)
| Pexp_construct ({txt = Longident.Lident "()"}, _) -> Unit
| Pexp_construct ({txt = Longident.Lident "true"}, _) -> Bool true
| Pexp_construct ({txt = Longident.Lident "false"}, _) -> Bool false
| Pexp_construct ({txt = Longident.Lident "[]"}, _) -> Nil
| Pexp_construct ({txt = Longident.Lident "::"}, Some ({pexp_desc = Pexp_tuple [e; e']})) ->
    Cons (of_real_ocaml e, of_real_ocaml e')
| Pexp_ident {txt = Longident.Lident "stdout"} -> OutChannel stdout
| Pexp_ident {txt = Longident.Lident "stderr"} -> OutChannel stderr
| Pexp_ident {txt = Longident.Lident "stdin"} -> InChannel stdin
| Pexp_ident {txt = v} -> Var (string_of_longident v)
| Pexp_ifthenelse (e, e1, Some e2) ->
    If (of_real_ocaml e, of_real_ocaml e1, of_real_ocaml e2)
| Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var {txt}}, exp) ->
    Fun (txt, of_real_ocaml exp)
| Pexp_function cases ->
    Function (List.map of_real_ocaml_case cases)
| Pexp_let (r, bindings, e') ->
    Let (r = Recursive, List.map of_real_ocaml_binding bindings, of_real_ocaml e')
| Pexp_apply
    ({pexp_desc = Pexp_ident {txt = Longident.Lident "raise"}},
     [(Nolabel, {pexp_desc = Pexp_construct ({txt = Longident.Lident s}, payload)})]) ->
         begin match payload with
           None -> Raise (s, None)
         | Some x -> Raise (s, Some (of_real_ocaml x))
         end
| Pexp_apply (* 2 operands *)
    ({pexp_desc = Pexp_ident {txt = Longident.Lident f}},
     [(Nolabel, l); (Nolabel, r)]) ->
       let e = of_real_ocaml l in
       let e' = of_real_ocaml r in
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
    App (of_real_ocaml e, of_real_ocaml e')
| Pexp_apply (e, apps) -> (* more than two operands *)
    of_real_ocaml_apps (List.rev (e::List.map snd apps))
| Pexp_sequence (e, e') ->
    Seq (of_real_ocaml e, of_real_ocaml e')
| Pexp_while (e, e') ->
    While (of_real_ocaml e, of_real_ocaml e', of_real_ocaml e, of_real_ocaml e')
| Pexp_for ({ppat_desc = Ppat_var {txt}}, e, e', flag, e'') ->
    let convert_flag = function Upto -> UpTo | Downto -> DownTo in
      For
        (txt, of_real_ocaml e, convert_flag flag,
         of_real_ocaml e', of_real_ocaml e'', of_real_ocaml e'')
| Pexp_record (items, _) ->
    Record (List.map of_real_ocaml_record_entry items)
| Pexp_field (e, {txt = Longident.Lident n}) ->
    Field (of_real_ocaml e, n)
| Pexp_setfield (e, {txt = Longident.Lident n}, e') ->
    SetField (of_real_ocaml e, n, of_real_ocaml e')
| Pexp_try
    (e, [{pc_lhs = {ppat_desc = Ppat_construct ({txt = Longident.Lident n}, _)}; pc_rhs}])
  ->
    TryWith (of_real_ocaml e, (n, of_real_ocaml pc_rhs))
| Pexp_tuple xs ->
    Tuple (List.map of_real_ocaml xs)
| Pexp_match (e, cases) ->
    Match (of_real_ocaml e, List.map of_real_ocaml_case cases)
| Pexp_assert e ->
    Assert (of_real_ocaml e)
| _ -> raise (UnknownNode "unknown node")

and of_real_ocaml_binding {pvb_pat = {ppat_desc}; pvb_expr} =
  (of_real_ocaml_pattern ppat_desc, of_real_ocaml pvb_expr)

and of_real_ocaml_apps = function
  [] -> assert false
| [x] -> of_real_ocaml x
| h::t -> App (of_real_ocaml_apps t, of_real_ocaml h)

and of_real_ocaml_record_entry = function
  ({txt = Longident.Lident n}, e) -> (n, ref (of_real_ocaml e))
| _ -> raise (UnknownNode "unknown record entry type")

and of_real_ocaml_case {pc_lhs; pc_guard; pc_rhs} =
  (of_real_ocaml_pattern pc_lhs.ppat_desc,
   begin match pc_guard with None -> None | Some x -> Some (of_real_ocaml x) end,
   of_real_ocaml pc_rhs)

and of_real_ocaml_pattern = function
  Ppat_var {txt} -> PatVar txt
| Ppat_constant (Pconst_integer (s, None)) -> PatInt (int_of_string s)
| Ppat_any -> PatAny
| Ppat_tuple patterns ->
    PatTuple
      (List.map of_real_ocaml_pattern (List.map (fun x -> x.ppat_desc) patterns))
| Ppat_construct ({txt = Longident.Lident "[]"}, _) -> PatNil
| Ppat_construct ({txt = Longident.Lident "()"}, _) -> PatUnit
| Ppat_construct ({txt = Longident.Lident "::"}, Some ({ppat_desc = Ppat_tuple [a; b]})) ->
    PatCons (of_real_ocaml_pattern a.ppat_desc, of_real_ocaml_pattern b.ppat_desc)
| Ppat_alias (pattern, {txt}) ->
    PatAlias (txt, of_real_ocaml_pattern pattern.ppat_desc)
| _ -> failwith "unknown pattern"

and of_real_ocaml x = of_real_ocaml_expression_desc x.pexp_desc

and of_real_ocaml_structure_item = function
  (* "1" or "let x = 1 in 2" *)
  {pstr_desc = Pstr_eval (e, _)} -> Some (of_real_ocaml e)
  (* let x = 1 *)
| {pstr_desc = Pstr_value (recflag, bindings)} ->
     Some (LetDef (recflag = Recursive, List.map of_real_ocaml_binding bindings))
  (* exception E of ... *)
| {pstr_desc = Pstr_exception {pext_name = {txt}; pext_kind = Pext_decl (t, _)}} ->
     Some (ExceptionDef (txt, t))
| {pstr_desc = Pstr_attribute _} -> None
| _ -> failwith "unknown structure item"

let of_real_ocaml x =
  Struct ("Main", Evalutils.option_map of_real_ocaml_structure_item x)

(* Recurse over the tinyocaml data type *)
let rec recurse f exp =
  match exp with
  | (Bool _ | Float _ | Var _ | Int _ | String _ | OutChannel _ | InChannel _ | Unit | Nil) as x -> x
  | Op (op, a, b) -> Op (op, f a, f b)
  | And (a, b) -> And (f a, f b)
  | Or (a, b) -> Or (f a, f b)
  | Cmp (cmp, a, b) -> Cmp (cmp, f a, f b)
  | If (e, e1, e2) -> If (f e, f e1, f e2)
  | Let (recflag, bindings, e) ->
      Let (recflag, List.map (fun (n, v) -> (n, f v)) bindings, recurse f e)
  | LetDef (recflag, bindings) ->
      LetDef (recflag, List.map (fun (n, v) -> (n, f v)) bindings)
  | Fun (n, fexp) -> Fun (n, f fexp)
  | App (a, b) -> App (f a, f b)
  | Seq (a, b) -> Seq (f a, f b)
  | While (a, b, c, d) -> While (f a, f b, f c, f d)
  | For (v, a, x, b, c, copy) -> For (v, f a, x, f b, f c, f copy) 
  | Control (c, x) -> Control (c, f x)
  | Record items ->
      List.iter (fun (k, v) -> v := f !v) items;
      Record items
  | Field (a, n) -> Field (f a, n)
  | SetField (a, n, b) -> SetField (f a, n, f b)
  | Raise s -> Raise s
  | TryWith (a, s) -> TryWith (f a, s)
  | ExceptionDef e -> ExceptionDef e
  | CallBuiltIn (name, args, fn) -> CallBuiltIn (name, List.map f args, fn)
  | Struct (n, l) -> Struct (n, List.map f l)
  | Cons (e, e') -> Cons (f e, f e')
  | Append (e, e') -> Append (f e, f e')
  | Match (e, patmatch) ->
      Match (e, List.map (recurse_case f) patmatch)
  | Function patmatch ->
      Function (List.map (recurse_case f) patmatch)
  | Tuple l -> Tuple (List.map f l)
  | Assert e -> Assert (f e)

and recurse_case f (pat, guard, rhs) =
  (pat,
   begin match guard with None -> None | Some g -> Some (f g) end,
   f rhs)

