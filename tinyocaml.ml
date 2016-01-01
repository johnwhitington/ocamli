open Parsetree
open Asttypes

type op = Add | Sub | Mul | Div

type cmp = LT | EQ | GT | EQLT | EQGT | NEQ

type t =
  Int of int                  (* 1 *)
| Bool of bool                (* false *)
| Var of string               (* x *)
| Op of (op * t * t)          (* + - / * *)
| And of (t * t)              (* && *)
| Or of (t * t)               (* || *)
| Cmp of (cmp * t * t)        (* < > <> = <= >= *)
| If of (t * t * t)           (* if e then e1 else e2 *)
| Let of (string * t * t)     (* let x = e in e' *)
| LetRec of (string * t * t)  (* let rec x = e in e' *)
| Fun of (string * t)         (* fun x -> e *)
| App of (t * t)              (* e e' *)

let op_to_string = function
  Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

let cmp_to_string = function
  LT -> "<" | EQ -> "=" | GT -> ">" | EQLT -> "<=" | EQGT -> ">=" | NEQ -> "<>"

(* For debug only. Very over-parenthesised *)
let rec to_string = function
  Int i -> string_of_int i
| Bool b -> string_of_bool b
| Var v -> v
| Op (op, l, r) ->
    Printf.sprintf "(%s %s %s)" (to_string l) (op_to_string op) (to_string r)
| And (l, r) ->
    Printf.sprintf "(%s && %s)" (to_string l) (to_string r)
| Or (l, r) ->
    Printf.sprintf "(%s || %s)" (to_string l) (to_string r)
| Cmp (cmp, l, r) ->
    Printf.sprintf "(%s %s %s)" (to_string l) (cmp_to_string cmp) (to_string r)
| If (e, e1, e2) ->
    Printf.sprintf "if (%s) then (%s) else (%s)"
      (to_string e) (to_string e1) (to_string e2)
| Let (v, e, e') ->
    Printf.sprintf "let %s = (%s) in (%s)" v (to_string e) (to_string e')
| LetRec (v, e, e') ->
    Printf.sprintf "let rec %s = (%s) in (%s)" v (to_string e) (to_string e')
| Fun (v, e) ->
    Printf.sprintf "fun %s -> (%s)" v (to_string e)
| App (e, e') ->
    Printf.sprintf "(%s) (%s)" (to_string e) (to_string e')

(* Convert from t to an OCaml parsetree. *)
let rec to_real_ocaml_expression_desc = function
  | Int i -> Pexp_constant (PConst_int (string_of_int i, None)) 
  | Bool b ->
      Pexp_construct
        ({txt = Longident.Lident (string_of_bool b); loc = Location.none},
          None)
  | Var v ->
      Pexp_ident {txt = Longident.Lident v; loc = Location.none}
  | Op (op, l, r) -> to_real_ocaml_apply l r (op_to_string op)
  | And (l, r) -> to_real_ocaml_apply l r "&&"
  | Or (l, r) -> to_real_ocaml_apply l r "||"
  | Cmp (cmp, l, r) -> to_real_ocaml_apply l r (cmp_to_string cmp)
  | If (e, e1, e2) ->
      Pexp_ifthenelse (to_real_ocaml e, to_real_ocaml e1, Some (to_real_ocaml e2))
  | Let (v, e, e') -> to_real_ocaml_let false v e e'
  | LetRec (v, e, e') -> to_real_ocaml_let true v e e'
  | Fun (v, e) ->
      let pattern =
       {ppat_desc = Ppat_var {txt = v; loc = Location.none};
        ppat_loc = Location.none;
        ppat_attributes = []};
      in
        Pexp_fun (Nolabel, None, pattern, to_real_ocaml e)
  | App (e, e') ->
      Pexp_apply (to_real_ocaml e, [(Nolabel, to_real_ocaml e')])

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

(* Convert from a parsetree to a t, assuming we can *)
let of_real_ocaml x = failwith "unimplemented"

