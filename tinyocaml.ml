open Parsetree
open Asttypes
open Ocamliutil

type op = Add | Sub | Mul | Div

type cmp = LT | EQ | GT | EQLT | EQGT | NEQ

type control = Underline | Bold

type forkind = UpTo | DownTo

type pattern =
  PatAny
| PatVar of string
| PatInt of int
| PatBool of bool
| PatInt32 of Int32.t
| PatInt64 of Int64.t
| PatNativeInt of Nativeint.t
| PatChar of char
| PatCharRange of char * char
| PatString of string
| PatUnit
| PatTuple of pattern list
| PatArray of pattern array
| PatNil
| PatCons of pattern * pattern
| PatAlias of string * pattern
| PatOr of pattern * pattern
| PatConstr of string * pattern option
| PatConstraint of pattern * Parsetree.core_type
| PatRecord of bool * (string * pattern) list
| PatException of pattern

and case = pattern * t option * t (* pattern, guard, rhs *)

and binding = pattern * t

and envitem = (* Environment items *)
  EnvBinding of bool * binding list ref
| EnvFunctor of string * string * modtype option * t * env (* name, input_module, name, modtype, e, env *)
| EnvType of (bool * Parsetree.type_declaration list)

and env = envitem list

and modtype = (* not final *)
  ModTypeSignature of t
| ModTypeIdent of string
| ModTypeWith of modtype * Parsetree.with_constraint list

and label = NoLabel | Labelled of string | Optional of string * t option

(* types, for %identity coercions *)
and typ =
  TypChar
| TypInt

and t =
(* values *)
  Unit                        (* () *)
| Int of int                  (* 1 *)
| Bool of bool                (* false *)
| Float of float              (* 1.0 *)
| String of string            (* "foo" *)
| OutChannel of out_channel   (* e.g stdout, stderr *)
| InChannel of in_channel     (* e.g stdin *)
| Record of (string * t ref) list  (* Records. *)
| Tuple of t list             (* (1, 2) *)
| Cons of (t * t)             (* List *)
| Nil                         (* [] *)
| Int32 of Int32.t            (* 1l *)
| Int64 of Int64.t            (* 1L *)
| NativeInt of Nativeint.t    (* 1n *)
| Char of char                (* 'a' *)
| Array of t array            (* [|1; 2; 3|] *)
| Constr of int * string * t option (* tag, Constructor, data *)
| Fun of (label * pattern * t * env)  (* fun x -> e *)
| Function of (case list * env)   
(* non-values *)
| Var of string               (* x *)
| Op of (op * t * t)          (* + - / * *)
| And of (t * t)              (* && *)
| Or of (t * t)               (* || *)
| Cmp of (cmp * t * t)        (* < > <> = <= >= *)
| If of (t * t * t option)    (* if e then e1 [else e2] *)
| Let of (bool * binding list * t) (* let x = e [and ...] in e' *)
| LetDef of (bool * binding list) (* let x = e [and ...] *)
| TypeDef of (bool * Parsetree.type_declaration list) (* type t = A | B of int [and t' = A' | B' of int] *)
| App of (t * t)              (* e e' *)
| Seq of (t * t)              (* e; e *)
| While of (t * t * t * t)    (* while e do e' done (e, e', copy_of_e copy_of_e') *)
| For of (string * t * forkind * t * t * t) (* for e [UpTo | DownTo] e' do e'' done (copy of e'') *)
| Field of (t * string)       (* e.y *)
| SetField of (t * string * t)(* e.y <- e' *)
| Raise of (string * t option)(* raise e *)
| Match of (t * case list)     (* match e with ... *)
| TryWith of (t * case list) (* try e with ... *)
| ExceptionDef of (string * Parsetree.constructor_arguments) (* Exception definition. *)
| Control of (control * t)    (* Control string for prettyprinting *)
| CallBuiltIn of (typ option * string * t list * (env -> t list -> t)) (* A built-in. Recieves args, returns result *)
| Struct of (bool * t list)   (* Module implementation. *)
| Sig of t list               (* Module signature. *)
| ModuleBinding of (string * t) (* Module M = ... *)
| ModuleConstraint of (modtype * t)  (* ME : MT *)
| ModuleIdentifier of string (* M *)
| ModuleApply of (t * t) (* M1 (M2) *)
| Functor of string * modtype option * t (* functor (X : MT) -> ME *)
| Append of (t * t)           (* @ *)
| Assert of t                 (* assert *)
| Open of string            (* open Unix followed by other things. *)
| LocalOpen of (string * t) (* String.(length "4") *)
| Include of t
| Lazy of t                 (* lazy t *)
| Annot of string * t * t   (* An annotation (name, payload, what is annotated *)


(* Iterate over the tinyocaml data type. The function will be called with each node of the tree *)
let rec iter f x =
  f x;
  match x with
  | (Bool _ | Float _ | Var _ | Int _ | Int32 _ | Int64 _ | NativeInt _
     | Char _ | String _ | OutChannel _ | InChannel _ | Unit | Nil |
     ModuleIdentifier _ | Raise (_, None) | ExceptionDef _ | TypeDef _ | Open _ ) -> ()
  | Control (_, a) | Field (a, _) | Raise (_, Some a) | TryWith (a, _)
  | ModuleBinding (_, a) | ModuleConstraint (_, a) | Functor (_, _, a) 
  | Assert a | Include a | LocalOpen (_, a) | Lazy a -> iter f a
  | Op (_, a, b) | And (a, b) | Or (a, b) | Cmp (_, a, b) | If (a, b, None)
  | App (a, b) | Seq (a, b) | Annot (_, a, b) | SetField (a, _, b)
  | ModuleApply (a, b) | Cons (a, b) | Append (a, b) -> iter f a; iter f b
  | If (e, e1, Some e2) -> iter f e; iter f e1; iter f e2
  | Let (recflag, bindings, e) -> List.iter (fun (_ , v) -> iter f v) bindings; iter f e
  | LetDef (recflag, bindings) -> List.iter (fun (_, v) -> iter f v) bindings
  | Fun (label, n, fexp, e) -> iter_label f label; iter f fexp
  | While (a, b, c, d) -> iter f a; iter f b; iter f c; iter f d
  | For (v, a, x, b, c, copy) -> iter f a; iter f b; iter f c; iter f copy
  | Array xs -> Array.iter (iter f) xs
  | Record items -> List.iter (fun (_, v) -> iter f !v) items
  | CallBuiltIn (_, _, args, _) -> List.iter f args
  | Struct (_, l) | Sig l | Tuple l -> List.iter (iter f) l
  | Function (patmatch, _) -> List.iter (iter_case f) patmatch
  | Constr (_, _, e) -> iter_option f e
  | Match (e, cases) -> iter f e; List.iter (iter_case f) cases

and iter_option f = function
  None -> ()
| Some x -> iter f x

and iter_case f (pat, guard, rhs) =
  begin match guard with None -> () | Some g -> f g end;
  f rhs

and iter_label f = function
  | Optional (s, Some x) -> iter f x
  | _ -> ()

(* Recurse over the tinyocaml data type *)
let rec recurse f exp =
  match exp with
  | (Bool _ | Float _ | Var _ | Int _ | Int32 _ | Int64 _ | NativeInt _
     | Char _ | String _ | OutChannel _ | InChannel _ | Unit | Nil |
     ModuleIdentifier _ ) as x -> x
  | Op (op, a, b) -> Op (op, f a, f b)
  | And (a, b) -> And (f a, f b)
  | Or (a, b) -> Or (f a, f b)
  | Cmp (cmp, a, b) -> Cmp (cmp, f a, f b)
  | If (e, e1, e2) -> If (f e, f e1, recurse_option f e2)
  | Let (recflag, bindings, e) ->
      Let (recflag, List.map (fun (n, v) -> (n, f v)) bindings, f e)
  | LetDef (recflag, bindings) ->
      LetDef (recflag, List.map (fun (n, v) -> (n, f v)) bindings)
  | Fun (label, n, fexp, env) -> Fun (recurse_label f label, n, f fexp, env)
  | App (a, b) -> App (f a, f b)
  | Seq (a, b) -> Seq (f a, f b)
  | While (a, b, c, d) -> While (f a, f b, f c, f d)
  | For (v, a, x, b, c, copy) -> For (v, f a, x, f b, f c, f copy) 
  | Control (c, x) -> Control (c, f x)
  | Annot (n, x, y) -> Annot (n, f x, f y)
  | Array xs ->
      Array.iteri (fun n c -> xs.(n) <- f c) xs;
      Array xs
  | Record items ->
      List.iter (fun (k, v) -> v := f !v) items;
      Record items
  | Field (a, n) -> Field (f a, n)
  | SetField (a, n, b) -> SetField (f a, n, f b)
  | Raise s -> Raise s (*FIXME *)
  | TryWith (a, s) -> TryWith (f a, s)
  | ExceptionDef e -> ExceptionDef e
  | TypeDef e -> TypeDef e
  | CallBuiltIn (typ, name, args, fn) -> CallBuiltIn (typ, name, List.map f args, fn)
  | Struct (b, l) -> Struct (b, List.map f l)
  | Sig l -> Sig (List.map f l)
  | ModuleBinding (n, m) -> ModuleBinding (n, f m)
  | ModuleConstraint (t, e) -> ModuleConstraint (t, f e)
  | ModuleApply (m1, m2) -> ModuleApply (f m1, f m2)
  | Functor (x, mt, me) -> Functor (x, mt, f me)
  | Cons (e, e') -> Cons (f e, f e')
  | Constr (tag, n, None) -> Constr (tag, n, None)
  | Constr (tag, n, Some t) -> Constr (tag, n, Some (f t))
  | Append (e, e') -> Append (f e, f e')
  | Match (e, patmatch) ->
      Match (e, List.map (recurse_case f) patmatch)
  | Function (patmatch, env) ->
      Function (List.map (recurse_case f) patmatch, env)
  | Tuple l -> Tuple (List.map f l)
  | Assert e -> Assert (f e)
  | Open n -> Open n
  | LocalOpen (n, e) -> LocalOpen (n, f e)
  | Lazy e -> Lazy (f e)
  | Include e -> Include (f e)

and recurse_label f = function
  | Optional (s, Some x) -> Optional (s, Some (f x))
  | x -> x

and recurse_option f = function
  None -> None
| Some x -> Some (f x)

and recurse_case f (pat, guard, rhs) =
  (pat,
   begin match guard with None -> None | Some g -> Some (f g) end,
   f rhs)

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

let rec dots_between = function
  [] -> ""
| [h] -> h
| h::t -> h ^ "." ^ dots_between t

let string_of_longident l =
  dots_between (Longident.flatten l)

let rec to_string = function
  Unit -> "Unit"
| Assert e -> Printf.sprintf "Assert %s" (to_string e)
| Int i -> Printf.sprintf "Int %i" i
| Int32 i -> Printf.sprintf "Int32 %li" i
| Int64 i -> Printf.sprintf "Int64 %Li" i
| NativeInt i -> Printf.sprintf "NativeInt %ni" i
| Bool b -> Printf.sprintf "Bool %b" b
| Float f -> Printf.sprintf "Float %f" f
| String s -> Printf.sprintf "String %s" s
| Char c -> Printf.sprintf "Char %C" c
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
| If (a, b, None) ->
    Printf.sprintf "If (%s, %s)" (to_string a) (to_string b)
| If (a, b, Some c) ->
    Printf.sprintf "If (%s, %s, %s)" (to_string a) (to_string b) (to_string c)
| Let (recflag, bindings, e') ->
    Printf.sprintf "%s (%s, %s)"
      (if recflag then "LetRec" else "Let") (to_string_bindings bindings) (to_string e')
| LetDef (recflag, bindings) ->
    Printf.sprintf "%s (%s)"
      (if recflag then "LetDefRec" else "LetDef") (to_string_bindings bindings)
| Fun (flabel, fname, fexp, fenv) ->
    Printf.sprintf "Fun (%s, %s, %s)"
      (to_string_label flabel) (to_string_pat fname) (to_string fexp)
      (*(to_string_env fenv)*)
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
| TypeDef (recflag, typedecls) ->
    Printf.sprintf "TypeDef (%b, %s)" recflag (to_string_typedecls typedecls)
| TryWith (e, patmatch) ->
    Printf.sprintf
      "TryWith (%s, %s)" (to_string e) (to_string_patmatch patmatch)
| Control (c, t) ->
    Printf.sprintf "Control (%s, %s)" (to_string_control c) (to_string t)
| Annot (n, p, t) ->
    Printf.sprintf "Annot (%s, %s, %s)" n (to_string p) (to_string t)
| CallBuiltIn (typ, name, _, _) ->
    Printf.sprintf "CallBuiltIn %s" name
| Struct l ->
    to_string_struct l
| Sig l ->
    to_string_sig l
| Constr (tag, n, None) ->
    Printf.sprintf "%s{%i}" n tag
| Constr (tag, n, Some t) ->
    Printf.sprintf "%s{%i} (%s)" n tag (to_string t)
| Cons (e, e') ->
    Printf.sprintf "Cons (%s, %s)" (to_string e) (to_string e')
| Nil -> "[]"
| Append (e, e') ->
    Printf.sprintf "Append (%s, %s)" (to_string e) (to_string e')
| Tuple xs ->
    Printf.sprintf
      "Tuple (%s)"
      (List.fold_left ( ^ ) "" (List.map (fun x -> to_string x ^ ", ") xs))
| Array xs ->
    Printf.sprintf
      "Array (%s)"
      (Array.fold_left ( ^ ) "" (Array.map (fun x -> to_string x ^ ", ") xs))
| Function (patmatch, env) ->
    Printf.sprintf "Function %s" (to_string_patmatch patmatch)
| Match (e, patmatch) ->
    Printf.sprintf
      "Match (%s, %s)" (to_string e) (to_string_patmatch patmatch)
| Open x ->
    Printf.sprintf "Open %s" x
| LocalOpen (x, e) ->
    Printf.sprintf "LocalOpen (%s, %s)" x (to_string e)
| ModuleBinding (m, t) ->
    Printf.sprintf "ModuleBinding (%s, %s)" m (to_string t)
| ModuleConstraint (modtype, t) ->
    Printf.sprintf "ModuleConstraint (%s, %s)"
      (to_string_modtype modtype) (to_string t)
| ModuleApply (m1, m2) ->
    Printf.sprintf "ModuleApply (%s, %s)"
      (to_string m1) (to_string m2)
| ModuleIdentifier x -> "ModuleIdentifier" ^ x
| Functor _ -> "Functor"
| Lazy e -> Printf.sprintf "Lazy (%s)" (to_string e)
| Include e -> Printf.sprintf "Include (%s)" (to_string e)

and to_string_label = function
  NoLabel -> "NoLabel"
| Labelled s -> Printf.sprintf "Labelled %s" s
| Optional (s, None) -> Printf.sprintf "Optional %s" s
| Optional (s, Some e) -> Printf.sprintf "Optional %s = %s\n" s (to_string e)

and to_string_modtype = function
  ModTypeSignature t ->
    Printf.sprintf "ModTypeSignature (%s)" (to_string t)
| ModTypeIdent s ->
    Printf.sprintf "ModTypeSignature (%s)" s
| ModTypeWith (mt, constraints) ->
    Printf.sprintf "ModTypeWith"

and to_string_bindings bs =
  List.fold_left ( ^ ) "" (List.map to_string_binding bs)

and to_string_binding (pat, e) =
  Printf.sprintf "%s = %s\n" (to_string_pat pat) (to_string e)

(* Just the names, because otherwise recursive bindings could never print... *)
and to_string_binding_names (pat, e) =
  Printf.sprintf "%s = ...\n" (to_string_pat pat)

and to_string_bindings_names bs =
  List.fold_left ( ^ ) "" (List.map to_string_binding_names bs)

and to_string_pat = function
  PatAny -> "_"
| PatVar v -> v
| PatInt i -> string_of_int i
| PatBool b -> string_of_bool b
| PatInt32 i -> Int32.to_string i
| PatInt64 i -> Int64.to_string i
| PatNativeInt i -> Nativeint.to_string i
| PatChar c -> Printf.sprintf "%C" c
| PatCharRange (c, c') -> Printf.sprintf "%C .. %C" c c'
| PatString s -> "\"" ^ String.escaped s ^ "\""
| PatUnit -> "()"
| PatTuple items ->
    "PatTuple (" ^ List.fold_left ( ^ ) "" (List.map to_string_pat items) ^ ")"
| PatNil -> "[]"
| PatCons _ -> "PatCons"
| PatAlias _ -> "PatAlias"
| PatOr _ -> "PatOr"
| PatConstraint _ -> "PatConstraint"
| PatArray _ -> "PatArray"
| PatConstr _ -> "PatConstr"
| PatRecord _ -> "PatRecord"
| PatException _ -> "PatException"

and to_string_patmatch xs =
  List.fold_left ( ^ ) "" (List.map (fun x -> to_string_case x ^ ", ") xs)

and to_string_guard = function
  None -> "None"
| Some g -> Printf.sprintf "Some (%s)" (to_string g)

and to_string_case (pat, guard, rhs) =
  Printf.sprintf "(%s, %s, %s)" (to_string_pat pat) (to_string_guard guard) (to_string rhs)

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

and to_string_struct (b, l) =
  Printf.sprintf "Struct [" ^
  List.fold_left ( ^ ) "" (List.map (fun x -> to_string x ^ "\n") l) ^
  "]"

and to_string_sig l =
  Printf.sprintf "Sig [" ^
  List.fold_left ( ^ ) "" (List.map (fun x -> to_string x ^ "\n") l) ^
  "]"

and to_string_core_type {ptyp_desc} =
  match ptyp_desc with
    Ptyp_any -> "_"
  | Ptyp_var s -> s
  | Ptyp_arrow (_, a, b) -> to_string_core_type a ^ " -> " ^ to_string_core_type b (* FIXME Prec/assoc? *)
  | Ptyp_tuple core_types ->
      "(" ^ List.fold_left ( ^ ) "" (List.map (fun x -> to_string_core_type x ^ ", ") core_types) ^ ")"
  | Ptyp_constr ({txt}, _) -> string_of_longident txt
  | Ptyp_object _ -> "Ptyp_object"
  | Ptyp_class _ -> "Ptyp_class"
  | Ptyp_alias _ -> "Ptyp_alias"
  | Ptyp_variant _ -> "Ptyp_variant"
  | Ptyp_poly _ -> "Ptyp_poly"
  | Ptyp_package _ -> "Ptyp_package"
  | Ptyp_extension _ -> "Ptyp_extenstion"

and to_string_constdecl {pcd_name = {txt}; pcd_args} =
  Printf.sprintf "%s %s" txt
  (match pcd_args with
  | Pcstr_tuple [] -> ""
  | Pcstr_tuple types -> "of" ^ List.fold_left (fun x y ->  x ^ " " ^ y) "" (List.map to_string_core_type types)
  | Pcstr_record _ -> "of RECORD")

and to_string_ptype_kind = function
  Ptype_variant constdecls ->
    List.fold_left (fun x y -> x ^ " | " ^ y) "" (List.map to_string_constdecl constdecls)
| _ -> "unknonwn ptype"

and to_string_typedecl {ptype_kind; ptype_name = {txt}} =
  Printf.sprintf "%s " txt ^ to_string_ptype_kind ptype_kind

and to_string_typedecls typedecls =
  List.fold_left ( ^ ) "" (List.map (fun x -> to_string_typedecl x ^ "\n") typedecls)

and to_string_envitem ?(full=false) = function
   EnvBinding (recflag, bs) ->
   Printf.sprintf "(%b, %s)\n"
     recflag
     ((if full then to_string_bindings else to_string_bindings_names) !bs)
 | EnvFunctor _ -> "EnvFunctor"
 | EnvType (recflag, typedecls) ->
     Printf.sprintf "EnvType (%b, %s)" recflag (to_string_typedecls typedecls)

and to_string_env ?(full=false) env =
  let strings = List.map to_string_envitem env in
    Printf.sprintf "Env [" ^ List.fold_left ( ^ ) "" strings ^ "]"

let rec bound_in_pattern = function
  PatAny -> []
| PatVar v -> [v]
| PatInt _ -> []
| PatBool _ -> []
| PatString _ -> []
| PatChar _ -> []
| PatCharRange (_, _) -> []
| PatInt32 _ -> []
| PatInt64 _ -> []
| PatNativeInt _ -> []
| PatUnit -> []
| PatTuple ls -> List.flatten (List.map bound_in_pattern ls)
| PatArray items -> List.flatten (List.map bound_in_pattern (Array.to_list items))
| PatNil -> []
| PatCons (h, t) -> bound_in_pattern h @ bound_in_pattern t
| PatAlias (a, p) -> a::bound_in_pattern p
| PatOr (a, b) -> bound_in_pattern a @ bound_in_pattern b
| PatConstr (_, None) -> []
| PatConstr (_, Some x) -> bound_in_pattern x
| PatConstraint (p, t) -> bound_in_pattern p
| PatRecord (_, ps) -> List.flatten (List.map bound_in_pattern (List.map snd ps))
| PatException p -> bound_in_pattern p

let bound_in_bindings bindings =
  List.flatten (List.map bound_in_pattern (List.map fst bindings))

(* Opening a module Find any items in the environment beginning with 'n', strip the name, and
duplicate them at the top of the environment. *)
let begins_with n s =
  String.length n <= String.length s &&
  n = String.sub s 0 (String.length n)

let rec pattern_begins_with n = function
  PatVar s when begins_with n s -> true
| PatTuple ts when List.for_all (pattern_begins_with n) ts -> true
| _ -> false

let binding_begins_with n (p, e) =
  pattern_begins_with n p

let string_loc_begins_with n ({txt} : string loc) =
  begins_with n txt

let bindings_beginning_with n env =
  option_map
    (function envitem ->
      match envitem with
        EnvFunctor (func_name, input_module_name, modtype, e, env) ->
          if begins_with n func_name
            then Some envitem
            else None
      | EnvBinding (recflag, bindings) ->
          if List.for_all (binding_begins_with n) !bindings
            then Some envitem
            else None
      | EnvType (recflag, typedecls) ->
          if List.for_all (string_loc_begins_with n) (List.map (fun x -> x.ptype_name) typedecls)
          then ((*Printf.printf "binding %s chosen\n" (to_string_envitem envitem);*) Some envitem)
          else ((*Printf.printf "binding %s NOT chosen\n" (to_string_envitem envitem);*) None))
    env

let cut n s =
  String.sub s (String.length n + 1) (String.length s - String.length n - 1)

let rec strip_pattern n = function
  PatVar s -> PatVar (cut n s)
| PatTuple ts -> PatTuple (List.map (strip_pattern n) ts)
| _ -> failwith "implement Ocamliutil.strip_pattern"

let strip_binding n (p, e) = (strip_pattern n p, e)

(* Strip the [n] from the type constructor name and it constructors *)
let strip_constructor n c =
  match c with {pcd_name = ({txt} as loc)} ->
    {c with pcd_name = {loc with txt = cut n txt}}

let strip_ptype_kind n = function
  Ptype_abstract -> Ptype_abstract
| Ptype_variant constructors -> Ptype_variant (List.map (strip_constructor n) constructors)
| Ptype_record record -> (* FIXME *) Ptype_record record
| Ptype_open -> Ptype_open

let strip_typedecl n t =
  {t with ptype_name = {t.ptype_name with txt = cut n t.ptype_name.txt};
          ptype_kind = strip_ptype_kind n t.ptype_kind}

let strip_bindings n = function
  EnvFunctor (s, input_module_name, modtype, e, env) ->
    EnvFunctor (cut n s, input_module_name, modtype, e, env)
| EnvBinding (recflag, bs) ->
    EnvBinding (recflag, ref (List.map (strip_binding n) !bs))
| EnvType (recflag, typedecls) ->
    EnvType (recflag, List.map (strip_typedecl n) typedecls)

let open_module n (env : env) =
  (*Printf.printf "open_module : %s\n" n;*)
  List.map (strip_bindings n) (bindings_beginning_with n env) @ env

let rec prefix_pattern prefix = function
  PatVar s -> PatVar (prefix ^ "." ^ s)
| PatTuple ts -> PatTuple (List.map (prefix_pattern prefix) ts)
| _ -> failwith "implement Ocamliutil.prefix_pattern"

let prefix_binding prefix (p, e) = (prefix_pattern prefix p, e)

let prefix_bindings p = function
  EnvBinding (recflag, bs) ->
    EnvBinding (recflag, ref (List.map (prefix_binding p) !bs))
| EnvFunctor (n, input_module_name, modtype, e, env) ->
    EnvFunctor (p ^ n, input_module_name, modtype, e, env)
| EnvType t -> EnvType t (* FIXME*)

(* For "module B = Bytes" Find any binding beginning with 'Bytes', replace
'Bytes' with 'B', and stick on to the front of the environment. *)
let alias_module current alias (env : env) =
  (*Printf.printf "Aliasing %s --> %s\n" current alias;*)
  let replaced =
    List.map
      (prefix_bindings alias)
      (List.map
        (strip_bindings current)
        (bindings_beginning_with current env))
  in
    replaced @ env

(* Assuming all the bindings are values already, add them to the environment as
Name.x, Name.y etc. *)
let bindings_of_struct_item p = function
  | LetDef (b, ld) -> Some (prefix_bindings p (EnvBinding (b, ref ld)))
  (*FIXME Add creation of EnvFunctor here, from a functor found in the struct. *)
  | _ -> None

let open_struct_as_module name items (env : env) =
  let bindings = option_map (bindings_of_struct_item name) items in
    let top_level_binding =
      EnvBinding (false, ref [(PatVar name, Struct (false, items))])
    in
      top_level_binding :: bindings @ env

