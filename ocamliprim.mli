val debug : bool ref

(* For now, we allow the continuation of the "emulated" primitives, to work
 * around the C interface not being finished yet. *)
val emulated : bool ref

val exe : string ref
val argv : string array ref

val lookup_primitive : ?typ:Parsetree.core_type -> string -> Tinyocaml.t

val of_real_ocaml : (Tinyocaml.env -> Parsetree.structure -> Tinyocaml.env * Tinyocaml.t) ref

val eval_until_value : (bool -> bool -> Tinyocaml.env -> Tinyocaml.t -> Tinyocaml.t) ref
