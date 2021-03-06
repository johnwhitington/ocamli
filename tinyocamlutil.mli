val calc : Tinyocaml.op -> (int -> int -> int)

val comp : Tinyocaml.cmp -> ('a -> 'a -> bool)

val is_value : Tinyocaml.t -> bool

val underline_redex : Tinyocaml.t -> Tinyocaml.t

val strip_control : Tinyocaml.t -> Tinyocaml.t

val remove_named_recursive_functions : bool -> string list -> Tinyocaml.t -> Tinyocaml.t

val fastcurry : bool ref

