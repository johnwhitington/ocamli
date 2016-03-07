val calc : Tinyocaml.op -> (int -> int -> int)

val comp : Tinyocaml.cmp -> (int -> int -> bool)

val is_value : Tinyocaml.t -> bool

val substitute : string -> Tinyocaml.t -> Tinyocaml.t -> Tinyocaml.t

val underline_redex : Tinyocaml.t -> Tinyocaml.t

val strip_control : Tinyocaml.t -> Tinyocaml.t

