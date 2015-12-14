(* The type of the internal state of the evaluator *)
type t

(* Initialise the evaluator with an OCaml ast *)
val init : Parsetree.structure -> t

type result =
    Next of t
  | IsValue
  | Malformed of string
  | Unimplemented of string

(* Evaluate one step. None if some problem occurs *)
val next : t -> result

val repr : t -> string

