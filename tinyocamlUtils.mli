val calc : Tinyocaml.op -> (int -> int -> int)

val comp : Tinyocaml.cmp -> (int -> int -> bool)

val is_value : Tinyocaml.t -> bool

val substitute : string -> Tinyocaml.t -> Tinyocaml.t -> Tinyocaml.t

