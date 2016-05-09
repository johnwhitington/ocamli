(** Main data structure and utility functions *)

(** Arithmetic operators *)
type op = Add | Sub | Mul | Div

(** Comparison operators *)
type cmp = LT | EQ | GT | EQLT | EQGT | NEQ

type control = Underline | Bold

type forkind = UpTo | DownTo

type pattern =
  PatAny
| PatVar of string
| PatTuple of pattern list

and case = pattern * t option * t (* pattern, guard, rhs *)

and patmatch = case list

and expatmatch = string * t (* for now *)

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
| Let of (pattern * t * t)      (** let x = e in e' *)
| LetRec of (pattern * t * t)   (** let rec x = e in e' *)
| LetDef of (pattern * t)       (** let x = e *)
| LetRecDef of (pattern * t)    (** let rec x = e *)
| Fun of (string * t)          (** fun x -> e *)
| App of (t * t)               (** e e' *)
| Seq of (t * t)               (** e; e *)
| While of (t * t * t * t)     (** while e do e' done (e, e', copy_of_e, copy_of_e') *)
| For of (string * t * forkind * t * t * t) (* for v = e [UpTo | DownTo] e' do e'' done, copy of e'' *)
| Field of (t * string)        (** e.y *)
| SetField of (t * string * t) (** e.y <- e' *)
| Raise of (string * t option) (** raise e *)
| Match of (t * patmatch)      (** match e with ... *)
| TryWith of (t * expatmatch)  (** try e with ... *)
| ExceptionDef of (string * Parsetree.constructor_arguments) (** exception e of ... *)
| Control of (control * t)     (** Control code *)
| CallBuiltIn of (string * t list * (t list -> t)) (** A built-in. Recieves args, returns result *)
| Struct of t list             (** Module implementation *)
| Sig of t list                (** Module signature *)
| Cons of t * t                (** :: *)
| Nil                          (** [] *)
| Append of t * t              (** @ *)
| Tuple of t list              (** (a, b) *)

val string_of_op : op -> string

val string_of_cmp : cmp -> string

val to_string : t -> string

(** Raised by [of_real_ocaml] if the program cannot be represented in tiny ocaml.*)
exception UnknownNode of string

(** Convert real ocaml to tiny ocaml, raising [UnknownNode] if not possible for
the given program *)
val of_real_ocaml : Parsetree.structure -> t

val recurse : (t -> t) -> t -> t

