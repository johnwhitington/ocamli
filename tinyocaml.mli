(** The Tinyocaml data structure *)

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
| EnvFunctor of string * string * modtype option * t * env
| EnvType of (bool * Parsetree.type_declaration list)

and env = envitem list

and modtype = (* not final *)
  ModTypeSignature of t
| ModTypeIdent of string
| ModTypeWith of modtype * Parsetree.with_constraint list

and label = NoLabel | Labelled of string | Optional of string * t option

and typ = TypChar | TypInt

(** The type of tiny-ocaml programs *)
and t =
  Unit                         (** () *)
| Int of int                   (** 1 *)
| Bool of bool                 (** false *)
| Float of float               (** 1.0 *)
| String of string             (** "foo" *)
| OutChannel of out_channel    (** e.g stdout *)
| InChannel of in_channel      (** e.g stdin *)
| Record of (string * t ref) list (** {a = e; b = e' ...} *)
| Tuple of t list              (** (a, b) *)
| Cons of (t * t)              (** :: *)
| Nil                          (** [] *)
| Int32 of Int32.t            (** 1l *)
| Int64 of Int64.t            (** 1L *)
| NativeInt of Nativeint.t    (** 1n *)
| Char of char                 (** 'a' *)
| Array of t array             (** [|1; 2; 3|] *)
| Constr of int * string * t option  (** tag, Constuctor [data] *)
| Fun of (label * pattern * t * env)          (** fun x -> e *)
| Function of (case list * env)       (** function x -> e | y -> f ... *)
| Var of string                (** x *)
| Op of (op * t * t)           (** + - / * *)
| And of (t * t)               (** && *)
| Or of (t * t)                (** || *)
| Cmp of (cmp * t * t)         (** < > <> = <= >= *)
| If of (t * t * t option)            (** if e then e1 [else e2] *)
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
| CallBuiltIn of (typ option * string * t list * (env -> t list -> t)) (** A built-in. Recieves args, returns result *)
| Struct of (bool * t list)  (** Module implementation. If bool is false, we don't print it (i.e it's the top level struct) *)
| Sig of t list                (** Module signature *)
| ModuleBinding of (string * t)(** Module M = ... *)
| ModuleConstraint of (modtype * t)  (** ME : MT *)
| ModuleIdentifier of string
| ModuleApply of (t * t)
| Functor of string * modtype option * t (* functor (X : MT) -> ME *)
| Append of (t * t)            (** @ *)
| Assert of t                  (** assert e *)
| Open of string               (** open Unix *)
| LocalOpen of (string * t)    (** String.(length "4") *)
| Include of t
| Lazy of t                    (** lazy e *)
| Annot of string * t * t      (** An annotation (name, payload, what is annotated) *)

val string_of_op : op -> string

val string_of_cmp : cmp -> string

val op_of_string : string -> op

val cmp_of_string : string -> cmp

val to_string : t -> string

val to_string_pat : pattern -> string

val recurse : (t -> t) -> t -> t

val iter : (t -> unit) -> t -> unit

val bound_in_pattern : pattern -> string list

val bound_in_bindings : binding list -> string list

val string_of_longident : Longident.t -> string

val to_string_bindings : binding list -> string

val to_string_envitem : ?full:bool -> envitem -> string

val to_string_env : ?full:bool -> env -> string

val open_module : string -> env -> env

val alias_module : string -> string -> env -> env

val open_struct_as_module : string -> t list -> env -> env

