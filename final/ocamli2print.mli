(* Flags *)
val syntax : bool ref
val syntax_tex : bool ref
val simple : bool ref
val width : int ref
val debug : bool ref
val showvals : bool ref

(* Debug printers for the basic types *)
val string_of_t : Ocamli2type.t -> string

val string_of_t' : Types.type_desc -> Ocamli2type.t' -> string

(* Print to a formatter. Put this back in once Tinyocaml gone... *)
val print : ?preamble:string -> Format.formatter -> Ocamli2type.t -> unit

val to_string : ?preamble:string -> Ocamli2type.t -> string

(* Print to a string *)
(*val to_string_from_finaltype : ?preamble:string -> Ocamli2type.t -> string*)

