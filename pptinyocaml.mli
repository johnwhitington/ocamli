val print : ?preamble:string -> Format.formatter -> Tinyocaml.t -> unit

val to_string : ?preamble:string -> Tinyocaml.t -> string

val simple : bool ref

val width : int ref

