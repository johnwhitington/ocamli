(* The type of the internal state of the evaluator *)
type t

(* Initialise the evaluator with an OCaml ast *)
val init : Parsetree.structure -> t

(* Evaluate one step. None if some problem occurs. *)
val next : t -> t Evalutils.result

(* The representation of the current state as a parse tree. *)
val tree : t -> Parsetree.structure

val to_string : t -> string

val tiny : t -> Tinyocaml.t

val last : unit -> Evalutils.last_op

