open Parsetree
open Asttypes
open Slist

type op = Add | Sub | Mul | Div

type cmp = LT | EQ | GT | EQLT | EQGT | NEQ

type control = Underline | Bold

type forkind = UpTo | DownTo

type pattern =
  PatAny
| PatVar of string
| PatInt of int
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
| Int32 of Int32.t            (* 1l *)
| Int64 of Int64.t            (* 1L *)
| NativeInt of Nativeint.t    (* 1n *)
| Bool of bool                (* false *)
| Float of float              (* 1.0 *)
| String of string            (* "foo" *)
| Char of char                (* 'a' *)
| OutChannel of out_channel   (* e.g stdout, stderr *)
| InChannel of in_channel     (* e.g stdin *)
| Array of t array            (* [|1; 2; 3|] *)
| Record of (string * t ref) list  (* Records. *)
| Tuple of t list             (* (1, 2) *)
| Constr of string * t option (* Constructor [data] *)
| Cons of (t * t)             (* List *)
| Nil                         (* [] *)
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
| CallBuiltIn of (typ option * string * t list * (t list -> t)) (* A built-in. Recieves args, returns result *)
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


(* The type of OCaml values in memory *)
type untyped_ocaml_value =
  UInt of int
| UBlock of int * untyped_ocaml_value array
| UString of string
| UDouble of float
| UDoubleArray of float array

external to_ocaml_value : t -> 'a = "to_ocaml_value"

external untyped_of_ocaml_value : 'a -> untyped_ocaml_value = "untyped_of_ocaml_value"

let rec read_untyped v typ =
  match v, typ.ptyp_desc with
  | UInt n, Ptyp_constr ({txt = Longident.Lident "int"}, _) ->
      Int n
  | UInt 0, Ptyp_constr ({txt = Longident.Lident "list"}, _) ->
      Nil
  | UString s, Ptyp_constr ({txt = Longident.Lident "string"}, _) ->
      String s
  | UDouble d, Ptyp_constr ({txt = Longident.Lident "float"}, _) ->
      Float d
  | UBlock (0, vs), Ptyp_tuple ts when Array.length vs = List.length ts ->
      Tuple (List.map2 read_untyped (Array.to_list vs) ts)
  | UBlock (0, [|h; t|]), Ptyp_constr ({txt = Longident.Lident "list"}, [elt_typ]) ->
      Cons (read_untyped h elt_typ, read_untyped t typ)
  | _ -> failwith "read_untyped: unimplemented"

let parse_type typ =
  typ |> Lexing.from_string |> Parse.core_type

let of_ocaml_value x typ =
  read_untyped (untyped_of_ocaml_value x) (parse_type typ)

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
  | Array xs -> Array (Array.map f xs)
  | Record items ->
      List.iter (fun (k, v) -> v := f !v) items;
      Record items
  | Field (a, n) -> Field (f a, n)
  | SetField (a, n, b) -> SetField (f a, n, f b)
  | Raise s -> Raise s
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
  | Constr (n, None) -> Constr (n, None)
  | Constr (n, Some t) -> Constr (n, Some (f t))
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
| TypeDef _ ->
    "TypeDef"
| TryWith (e, patmatch) ->
    Printf.sprintf
      "TryWith (%s, %s)" (to_string e) (to_string_patmatch patmatch)
| Control (c, t) ->
    Printf.sprintf "Control (%s, %s)" (to_string_control c) (to_string t)
| CallBuiltIn (typ, name, _, _) ->
    Printf.sprintf "CallBuiltIn %s" name
| Struct l ->
    to_string_struct l
| Sig l ->
    to_string_sig l
| Constr (n, None) ->
    Printf.sprintf "%s" n
| Constr (n, Some t) ->
    Printf.sprintf "%s (%s)" n (to_string t)
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
  
and to_string_env ?(full=false) env =
  let strings = 
    List.map
      (function
         EnvBinding (recflag, bs) ->
         Printf.sprintf "(%b, %s)\n"
           recflag
           ((if full then to_string_bindings else to_string_bindings_names) !bs)
       | EnvFunctor _ -> "EnvFunctor")
      env
  in
    Printf.sprintf "Env [" ^ List.fold_left ( ^ ) "" strings ^ "]"

let rec dots_between = function
  [] -> ""
| [h] -> h
| h::t -> h ^ "." ^ dots_between t

let string_of_longident l =
  dots_between (Longident.flatten l)

let rec bound_in_pattern = function
  PatAny -> []
| PatVar v -> [v]
| PatInt _ -> []
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

