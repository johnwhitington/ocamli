val print : ?preamble:string -> Format.formatter -> Tinyocaml.t -> unit

val to_string : ?preamble:string -> Tinyocaml.t -> string

val syntax : bool ref

val syntax_tex : bool ref

val simple : bool ref

val width : int ref

val fastcurry : bool ref

val debug : bool ref

