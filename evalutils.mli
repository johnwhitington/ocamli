(* For writing evalutors with *)
type 'a result =
    Next of 'a
  | IsValue
  | Malformed of string
  | Unimplemented of string

exception ExnUnimplemented of string

exception ExnMalformed of string

val unimp : string -> 'a

val malformed : string -> 'a

val to_string : Parsetree.expression -> string

val getexpr : Parsetree.structure -> Parsetree.expression

val with_desc : Parsetree.expression_desc -> Parsetree.expression

val bool_of_bool_value : Parsetree.expression -> bool

val mkbool : bool -> Parsetree.expression


