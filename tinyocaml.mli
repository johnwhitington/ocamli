(** Main data structure and utility functions *)

(** Arithmetic operators *)
type op = Add | Sub | Mul | Div

(** Comparison operators *)
type cmp = LT | EQ | GT | EQLT | EQGT | NEQ

type ex = string (* for now *)

type control = Underline | Bold | Pervasive

type forkind = UpTo | DownTo

type patmatch = string * t (* for now *)

(** The type of tiny-ocaml programs *)
and t =
  Unit                         (** () *)
| Int of int                   (** 1 *)
| Bool of bool                 (** false *)
| Float of float               (** 1.0 *)
| String of string             (** "foo" *)
| OutChannel of out_channel    (** e.g stdout *)
| InChannel of in_channel      (** e.g stdin *)
| Var of string                (** x *)
| Record of (string * t ref) list (** {a = e; b = e' ...} *)
| Op of (op * t * t)           (** + - / * *)
| And of (t * t)               (** && *)
| Or of (t * t)                (** || *)
| Cmp of (cmp * t * t)         (** < > <> = <= >= *)
| If of (t * t * t)            (** if e then e1 else e2 *)
| Let of (string * t * t)      (** let x = e in e' *)
| LetRec of (string * t * t)   (** let rec x = e in e' *)
| Fun of (string * t)          (** fun x -> e *)
| App of (t * t)               (** e e' *)
| Seq of (t * t)               (** e; e *)
| While of (t * t * t * t)     (** while e do e' done (e, e', copy_of_e, copy_of_e') *)
| For of (string * t * forkind * t * t * t) (* for v = e [UpTo | DownTo] e' do e'' done, copy of e'' *)
| Field of (t * string)        (** e.y *)
| SetField of (t * string * t) (** e.y <- e' *)
| Raise of ex                  (** raise e *)
| TryWith of (t * patmatch)    (** try e with ... *)
| Control of (control * t)     (** Control code *)
| CallBuiltIn of (t list * (t list -> t)) (** A built-in. Recieves args, returns result *)
| Module of t list

val string_of_op : op -> string

val string_of_cmp : cmp -> string

(** Convert tiny ocaml to real ocaml. *)
val to_real_ocaml : t -> Parsetree.expression

(** Raised by [of_real_ocaml] if the program cannot be represented in tiny ocaml
.*)
exception UnknownNode of string

(** Convert real ocaml to tiny ocaml, raising [UnknownNode] if not possible for
the given program *)
val of_real_ocaml : Parsetree.structure -> t

val recurse : (t -> t) -> t -> t

