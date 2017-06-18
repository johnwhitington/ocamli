val to_ocaml_value : Tinyocaml.t -> 'a

val of_ocaml_value : Tinyocaml.env -> 'a -> string -> Tinyocaml.t

type untyped_ocaml_value =
  UInt of int
| UBlock of int * untyped_ocaml_value array
| UString of string
| UDouble of float
| UDoubleArray of float array

val untyped_of_ocaml_value : 'a -> untyped_ocaml_value

