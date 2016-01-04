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

val to_string : t -> string

val to_real_ocaml : t -> Parsetree.expression

exception UnknownNode of string

val of_real_ocaml : Parsetree.expression -> t



