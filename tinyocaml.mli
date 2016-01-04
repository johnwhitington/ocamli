(** Main data structure and utility functions *)

(** Arithmetic operators *)
type op = Add | Sub | Mul | Div

(** Comparison operators *)
type cmp = LT | EQ | GT | EQLT | EQGT | NEQ

(** The type of tiny-ocaml programs *)
type t =
  Int of int                  (** 1 *)
| Bool of bool                (** false *)
| Var of string               (** x *)
| Op of (op * t * t)          (** + - / * *)
| And of (t * t)              (** && *)
| Or of (t * t)               (** || *)
| Cmp of (cmp * t * t)        (** < > <> = <= >= *)
| If of (t * t * t)           (** if e then e1 else e2 *)
| Let of (string * t * t)     (** let x = e in e' *)
| LetRec of (string * t * t)  (** let rec x = e in e' *)
| Fun of (string * t)         (** fun x -> e *)
| App of (t * t)              (** e e' *)

(** Very basic prettyprinter, for debug only. *)
val to_string : t -> string

(** Convert tiny ocaml to real ocaml. *)
val to_real_ocaml : t -> Parsetree.expression

(** Raised by [of_real_ocaml] if the program cannot be represented in tiny ocaml
.*)
exception UnknownNode of string

(** Convert real ocaml to tiny ocaml, raising [UnknownNode] if not possible for
the given program *)
val of_real_ocaml : Parsetree.expression -> t



