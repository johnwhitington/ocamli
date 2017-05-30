val debug : bool ref

val exe : string ref
val argv : string array ref

val lookup_primitive : Parsetree.core_type -> string -> Tinyocaml.t

val of_real_ocaml : (Tinyocaml.env -> Parsetree.structure -> Tinyocaml.t) ref

