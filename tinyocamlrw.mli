(** Raised by [of_real_ocaml] if the program cannot be represented in tiny ocaml.*)
exception UnknownNode of string

val realops : bool ref

(** Convert real ocaml to tiny ocaml, raising [UnknownNode] if not possible for
the given program *)
val of_real_ocaml : Tinyocaml.env -> Parsetree.structure -> Tinyocaml.env * Tinyocaml.t

val to_real_ocaml : Tinyocaml.t -> Parsetree.structure

(* Quick & nasty for top level. Removes the outside struct, returns env, removes let _ = of final. *)
val of_string : string -> Tinyocaml.env * Tinyocaml.t
