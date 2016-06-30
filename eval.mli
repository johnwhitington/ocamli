(* The type of the internal state of the evaluator *)
type t

exception ExceptionRaised of string * Tinyocaml.t option

(* Initialise the evaluator with an OCaml ast *)
val init : Parsetree.structure -> t

val init_from_tinyocaml : Tinyocaml.t -> t

(* Evaluate one step. None if some problem occurs. *)
val next : t -> t Ocamliutil.result

(* The representation of the current state as a parse tree. *)
(*val tree : t -> Parsetree.structure*)

(* The prettyprinted string of the current state *)
val to_string : t -> string

val tiny : t -> Tinyocaml.t

val last : unit -> Ocamliutil.last_op list

val debug : bool ref

val peek : t -> Ocamliutil.last_op list

val newlines : t -> bool

val fastcurry : bool ref

val dopeek : bool ref

val docollectunusedlets : bool ref

