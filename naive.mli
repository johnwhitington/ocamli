(* The type of the internal state of the evaluator *)
type t

(* Initialise the evaluator with an OCaml ast *)
val init : Parsetree.structure -> t

(* Evaluate one step. None if some problem occurs *)
val next : t -> t Evalutils.result

val repr : t -> string

