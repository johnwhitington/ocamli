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
| PatInt of int
| PatInt32 of Int32.t
| PatInt64 of Int64.t
| PatNativeInt of Nativeint.t
| PatChar of char
| PatCharRange of char * char
| PatString of string
| PatUnit
| PatTuple of pattern list
| PatNil
| PatCons of pattern * pattern
| PatAlias of string * pattern
| PatOr of pattern * pattern
| PatConstr of string * pattern option

and case = pattern * t option * t (* pattern, guard, rhs *)

and binding = pattern * t

and env = (bool * binding list) list

(** The type of tiny-ocaml programs *)
and t =
  Unit                         (** () *)
| Int of int                   (** 1 *)
| Int32 of Int32.t            (** 1l *)
| Int64 of Int64.t            (** 1L *)
| NativeInt of Nativeint.t    (** 1n *)
| Bool of bool                 (** false *)
| Float of float               (** 1.0 *)
| String of string             (** "foo" *)
| Char of char                 (** 'a' *)
| OutChannel of out_channel    (** e.g stdout *)
| InChannel of in_channel      (** e.g stdin *)
| Record of (string * t ref) list (** {a = e; b = e' ...} *)
| Tuple of t list              (** (a, b) *)
| Constr of string * t option  (** Constuctor [data] *)
| Cons of (t * t)              (** :: *)
| Nil                          (** [] *)
| Fun of (pattern * t * env)          (** fun x -> e *)
| Function of (case list * env)       (** function x -> e | y -> f ... *)
| Var of string                (** x *)
| Op of (op * t * t)           (** + - / * *)
| And of (t * t)               (** && *)
| Or of (t * t)                (** || *)
| Cmp of (cmp * t * t)         (** < > <> = <= >= *)
| If of (t * t * t)            (** if e then e1 else e2 *)
| Let of (bool * binding list * t) (** let x = e in e' *)
| LetDef of (bool * binding list)  (** let x = e *)
| TypeDef of (bool * Parsetree.type_declaration list) (* type t = A | B of int *)
| App of (t * t)               (** e e' *)
| Seq of (t * t)               (** e; e *)
| While of (t * t * t * t)     (** while e do e' done (e, e', copy_of_e, copy_of_e') *)
| For of (string * t * forkind * t * t * t) (* for v = e [UpTo | DownTo] e' do e'' done, copy of e'' *)
| Field of (t * string)        (** e.y *)
| SetField of (t * string * t) (** e.y <- e' *)
| Raise of (string * t option) (** raise e *)
| Match of (t * case list)      (** match e with ... *)
| TryWith of (t * case list)  (** try e with ... *)
| ExceptionDef of (string * Parsetree.constructor_arguments) (** exception e of ... *)
| Control of (control * t)     (** Control code *)
| CallBuiltIn of (string * t list * (t list -> t)) (** A built-in. Recieves args, returns result *)
| Struct of (string * t list)  (** Module implementation *)
| Sig of t list                (** Module signature *)
| Append of (t * t)              (** @ *)
| Assert of t                  (** assert e *)

val string_of_op : op -> string

val string_of_cmp : cmp -> string

val to_string : t -> string

val to_ocaml_value : t -> 'a

val of_ocaml_value : 'a -> string -> t

type untyped_ocaml_value =
  UInt of int
| UBlock of int * untyped_ocaml_value array
| UString of string
| UDouble of float
| UDoubleArray of float array

val untyped_of_ocaml_value : 'a -> untyped_ocaml_value

(** Raised by [of_real_ocaml] if the program cannot be represented in tiny ocaml.*)
exception UnknownNode of string

(** Convert real ocaml to tiny ocaml, raising [UnknownNode] if not possible for
the given program *)
val of_real_ocaml : Parsetree.structure -> t

val to_real_ocaml : t -> Parsetree.structure

val recurse : (t -> t) -> t -> t

val bound_in_pattern : pattern -> string list

