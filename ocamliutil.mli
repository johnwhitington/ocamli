(* For writing evalutors with *)
type 'a result =
    Next of 'a
  | IsValue
  | Malformed of string
  | Unimplemented of string

val typecheck : bool ref

exception ExnUnimplemented of string

exception ExnMalformed of string

type last_op =
    Arith
  | Boolean
  | Comparison
  | IfBool
  | InsidePervasive

val unimp : string -> 'a

val malformed : string -> 'a

val ast_to_string : Parsetree.expression -> string

val with_desc : Parsetree.expression_desc -> Parsetree.expression

val isstarred : string -> bool

val star : string -> string

val unstar : string -> string

val option_map : ('a -> 'b option) -> 'a list -> 'b list

val load_file : string -> string

val env : Env.t

val ast : string -> Parsetree.structure

val open_module : string -> Tinyocaml.env -> Tinyocaml.env

