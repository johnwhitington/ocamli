open Parsetree
open Asttypes

type op = Add | Sub | Mul | Div

type cmp = LT | EQ | GT | EQLT | EQGT | NEQ

type ex = string (* for now *)

type control = Underline | Bold

type forkind = UpTo | DownTo

type patmatch = string * t (* for now *)

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
| Let of (string * t * t)     (* let x = e in e' *)
| LetRec of (string * t * t)  (* let rec x = e in e' *)
| LetDef of (string * t)      (* let x = e *)
| LetRecDef of (string * t)   (* let rec x = e *)
| Fun of {fname : string; fexp : t; fper : bool}  (* fun x -> e FIXME: Do we need fper now we have __PER__? *)
| App of (t * t)              (* e e' *)
| Seq of (t * t)              (* e; e *)
| While of (t * t * t * t)    (* while e do e' done (e, e', copy_of_e copy_of_e') *)
| For of (string * t * forkind * t * t * t) (* for e [UpTo | DownTo] e' do e'' done (copy of e'') *)
| Field of (t * string)       (* e.y *)
| SetField of (t * string * t)(* e.y <- e' *)
| Raise of ex                 (** raise e *)
| TryWith of (t * patmatch)   (** try e with ... *)
| Control of (control * t)    (* Control string for prettyprinting *)
| CallBuiltIn of (string * t list * (t list -> t)) (** A built-in. Recieves args, returns result *)
| Module of t list

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

let rec to_string = function
  Unit -> "Unit"
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
| Let (x, e, e') ->
    Printf.sprintf "Let (%s, %s, %s)" x (to_string e) (to_string e')
| LetRec (x, e, e') ->
    Printf.sprintf "LetRec (%s, %s, %s)" x (to_string e) (to_string e')
| LetDef (x, e) ->
    Printf.sprintf "LetDef (%s, %s)" x (to_string e)
| LetRecDef (x, e) ->
    Printf.sprintf "LetRecDef (%s, %s)" x (to_string e)
| Fun {fname; fexp; fper} ->
    Printf.sprintf "Fun {fname = %s, fexp = %s, fper = %b}" fname (to_string fexp) fper
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
| Raise ex ->
    Printf.sprintf "Raise %s" (to_string_exn ex)
| TryWith (t, pat) ->
    Printf.sprintf "TryWith (%s, %s)" (to_string t) (to_string_patmatch pat)
| Control (c, t) ->
    Printf.sprintf "Control (%s, %s)" (to_string_control c) (to_string t)
| CallBuiltIn (name, _, _) ->
    Printf.sprintf "CallBuiltIn %s" name
| Module l ->
    to_string_module l

and to_string_control = function
  Underline -> "Underline"
| Bold -> "Bold"

and to_string_exn = function
  s -> s

and to_string_patmatch (str, t) =
  Printf.sprintf "(%s, %s)" str (to_string t)

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

and to_string_module l =
  "Module [" ^ List.fold_left ( ^ ) "" (List.map (fun x -> to_string x ^ "\n") l) ^ "]"

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
  | If (e, e1, e2) ->
      Pexp_ifthenelse (to_real_ocaml e, to_real_ocaml e1, Some (to_real_ocaml e2))
  | Let (v, e, e') -> to_real_ocaml_let false v e e'
  | LetRec (v, e, e') -> to_real_ocaml_let true v e e'
  | Fun {fname; fexp} ->
      let pattern =
       {ppat_desc = Ppat_var {txt = fname; loc = Location.none};
        ppat_loc = Location.none;
        ppat_attributes = []};
      in
        Pexp_fun (Nolabel, None, pattern, to_real_ocaml fexp)
  | App (e, e') ->
      Pexp_apply (to_real_ocaml e, [(Nolabel, to_real_ocaml e')])
  | Seq (e, e') ->
      Pexp_sequence (to_real_ocaml e, to_real_ocaml e') 

and to_real_ocaml_let r v e e' =
  let binding =
     {pvb_pat =
       {ppat_desc = Ppat_var {txt = v; loc = Location.none};
        ppat_loc = Location.none;
        ppat_attributes = []};
      pvb_expr = to_real_ocaml e;
      pvb_attributes = [];
      pvb_loc = Location.none};
  in
    Pexp_let
      ((if r then Recursive else Nonrecursive), [binding], to_real_ocaml e')

and to_real_ocaml_apply l r n =
  let exprs =
    [(Nolabel, to_real_ocaml l); (Nolabel, to_real_ocaml r)] in
  let expr =
    Evalutils.with_desc
      (Pexp_ident
         {txt = Longident.Lident n; loc = Location.none})
  in
    Pexp_apply (expr, exprs)

and to_real_ocaml x =
  Evalutils.with_desc (to_real_ocaml_expression_desc x)

exception UnknownNode of string

let allper = ref false

(* Convert from a parsetree to a t, assuming we can *)
let rec of_real_ocaml_expression_desc = function
  Pexp_constant (Pconst_integer (s, None)) -> Int (int_of_string s)
| Pexp_constant (Pconst_string (s, None)) -> String s
| Pexp_constant (Pconst_float (s, None)) -> Float (float_of_string s)
| Pexp_construct ({txt = Longident.Lident "()"}, _) -> Unit
| Pexp_construct ({txt = Longident.Lident "true"}, _) -> Bool true
| Pexp_construct ({txt = Longident.Lident "false"}, _) -> Bool false
| Pexp_ident {txt = Longident.Lident "stdout"} -> OutChannel stdout
| Pexp_ident {txt = Longident.Lident "stderr"} -> OutChannel stderr
| Pexp_ident {txt = Longident.Lident "stdin"} -> InChannel stdin
| Pexp_ident {txt = Longident.Lident v} -> Var v
| Pexp_ifthenelse (e, e1, Some e2) ->
    If (of_real_ocaml e, of_real_ocaml e1, of_real_ocaml e2)
| Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var {txt}}, exp) ->
    Fun {fname = txt; fexp = of_real_ocaml exp; fper = false || !allper}
| Pexp_let
    (r, [{pvb_pat = {ppat_desc = Ppat_var {txt}}; pvb_expr}], e') ->
       if r = Recursive
         then LetRec (txt, of_real_ocaml pvb_expr, of_real_ocaml e')
         else Let (txt, of_real_ocaml pvb_expr, of_real_ocaml e')
(* This is raise. We special case for now, since we don't have exception
 * definitions *)
| Pexp_apply
    ({pexp_desc = Pexp_ident {txt = Longident.Lident "raise"}},
     [(Nolabel, {pexp_desc = Pexp_construct ({txt = Longident.Lident s}, _)})]) ->
       Raise s
| Pexp_apply (* 2 opearands *)
    ({pexp_desc = Pexp_ident {txt = Longident.Lident f}},
     [(Nolabel, l); (Nolabel, r)]) ->
       let e = of_real_ocaml l in
       let e' = of_real_ocaml r in
         begin match f with
           "&&" -> And (e, e')
         | "||" -> Or (e, e')
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
| _ -> raise (UnknownNode "unknown node")

and of_real_ocaml_apps = function
  [] -> assert false
| [x] -> of_real_ocaml x
| h::t -> App (of_real_ocaml_apps t, of_real_ocaml h)

and of_real_ocaml_record_entry = function
  ({txt = Longident.Lident n}, e) -> (n, ref (of_real_ocaml e))
| _ -> raise (UnknownNode "unknown record entry type")

and of_real_ocaml x = of_real_ocaml_expression_desc x.pexp_desc

and of_real_ocaml_structure_item = function
  (* "1" or "let x = 1 in 2" *)
  {pstr_desc = Pstr_eval (e, _)} -> of_real_ocaml e
  (* let x = 1 *)
| {pstr_desc = Pstr_value (recflag, [{pvb_pat = {ppat_desc = Ppat_var {txt}}; pvb_expr = e}])} ->
    if recflag = Nonrecursive then
      LetDef (txt, of_real_ocaml e) 
    else
      LetRecDef (txt, of_real_ocaml e)
  (* let _ = 1 *)
| {pstr_desc = Pstr_value (recflag, [{pvb_pat = {ppat_desc = Ppat_any}; pvb_expr = e}])} ->
    if recflag = Nonrecursive then
      LetDef ("_", of_real_ocaml e) 
    else
      LetRecDef ("_", of_real_ocaml e)
  (* let () = ... *)
| {pstr_desc = Pstr_value (recflag, [{pvb_pat = {ppat_desc = Ppat_construct ({txt}, None)}; pvb_expr = e}])} ->
    if recflag = Nonrecursive then
      LetDef ("()", of_real_ocaml e) 
    else
      LetRecDef ("()", of_real_ocaml e)
| _ -> failwith "unknown structure item"

let of_real_ocaml ?(allpervasive = false) x =
  allper := allpervasive;
  Module (List.map of_real_ocaml_structure_item x)

(* Recurse over the tinyocaml data type *)
let rec recurse f = function
| (Bool _ | Float _ | Var _ | Int _ | String _ | OutChannel _ | InChannel _ | Unit) as x -> x
| Op (op, a, b) -> Op (op, f a, f b)
| And (a, b) -> And (f a, f b)
| Or (a, b) -> Or (f a, f b)
| Cmp (cmp, a, b) -> Cmp (cmp, f a, f b)
| If (e, e1, e2) -> If (f e, f e1, f e2)
| Let (n, v, e) -> Let (n, f v, f e)
| LetRec (n, v, e) -> LetRec (n, f v, f e)
| LetDef (n, v) -> LetDef (n, f v)
| LetRecDef (n, v) -> LetRecDef (n, f v)
| Fun ({fexp} as r) -> Fun {r with fexp = f fexp}
| App (a, b) -> App (f a, f b)
| Seq (a, b) -> Seq (f a, f b)
| While (a, b, c, d) -> While (f a, f b, f c, f d)
| For (v, a, x, b, c, copy) -> For (v, f a, x, f b, f c, f copy) 
| Control (c, x) -> Control (c, f x)
| Record items ->
    List.iter (fun (k, v) -> v := f !v) items;
    Record items
| Field (a, n) -> Field (recurse f a, n)
| SetField (a, n, b) -> SetField (f a, n, f b)
| Raise s -> Raise s
| TryWith (a, s) -> TryWith (f a, s)
| CallBuiltIn (name, args, fn) -> CallBuiltIn (name, List.map f args, fn)
| Module l -> Module (List.map f l)

