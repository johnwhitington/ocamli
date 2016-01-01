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

(* FIXME: a) Correct parenthesisation b) Minimal correct parnenthesisation c)
Use Format module.  *)
let rec to_string = function
  Int i -> string_of_int i
| Bool b -> string_of_bool b
| Var s -> s
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
    Printf.sprintf "let %s = %s in %s" v (to_string e) (to_string e')
| LetRec (v, e, e') ->
    Printf.sprintf "let rec %s = %s in %s" v (to_string e) (to_string e')
| Fun (v, e) ->
    Printf.sprintf "fun %s -> (%s)" v (to_string e)
| App (e, e') ->
    Printf.sprintf "(%s) (%s)" (to_string e) (to_string e')

(* Convert from t to an OCaml parsetree. *)
let to_real_ocaml_expression_desc = function
  | Int i -> Pexp_constant (Const_int i) 
  | Bool b ->
      Pexp_construct
        ({txt = Longident.Lident (string_of_bool b); loc = Location.none},
          None)
  | _ -> failwith "unimplemented"

let to_real_ocaml x = failwith "unimplemented"

let of_real_ocaml x = failwith "unimplemented"

