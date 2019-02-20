(* Flags *)
val syntax : bool ref
val syntax_tex : bool ref
val showvals : bool ref
val show_all_lets : bool ref
val string_of_t_show_types : bool ref

(* Debug printers for the basic types *)
val string_of_t : Ocamli2type.t -> string

val string_of_t' : Types.type_desc -> Ocamli2type.t' -> string

(* Print to a formatter. *)
val print : Format.formatter -> Ocamli2type.t -> unit

(* Print to a string *)
val to_string : Ocamli2type.t -> string

