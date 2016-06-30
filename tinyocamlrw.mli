(** Raised by [of_real_ocaml] if the program cannot be represented in tiny ocaml.*)
exception UnknownNode of string

(** Convert real ocaml to tiny ocaml, raising [UnknownNode] if not possible for
the given program *)
val of_real_ocaml : Parsetree.structure -> Tinyocaml.t

val to_real_ocaml : Tinyocaml.t -> Parsetree.structure

