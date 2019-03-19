(* Flags *)
val syntax : bool ref
val syntax_tex : bool ref
val showvals : bool ref
val show_all_lets : bool ref
val string_of_t_show_types : bool ref
val printas : bool ref

(* Debug printers for the basic types *)
val string_of_t : Type.t -> string

val string_of_t' : Types.type_expr -> Type.t' -> string

val string_of_pattern : Type.pattern -> string

val string_of_ocaml_type : Types.type_expr -> string

(* Print to a formatter. *)
val print : Format.formatter -> Type.t -> unit

(* Print to a string *)
val to_string : Type.t -> string

