val silent : bool ref
val quiet : bool ref
val top : bool ref
val debug : bool ref
val showpervasives : bool ref
val machine : string ref
val printer : string ref
val width : int ref
val show_simple_arithmetic : bool ref
val debugtiny : bool ref
val debugpp : bool ref
val prompt : bool ref
val step : float ref
val fastcurry : bool ref
type mode = FromFile of string | FromText of string
val source : mode option ref
val setfile : string -> unit
val settext : string -> unit

module type Evaluator =
  sig
    type t
    val init : Parsetree.structure -> t
    val next : t -> t Ocamliutil.result
    val tiny : t -> Tinyocaml.t
    val to_string : t -> string
    val last : unit -> Ocamliutil.last_op list
    val peek : t -> Ocamliutil.last_op list
    val newlines : t -> bool
    val fastcurry : bool ref
  end
val implementations : (string * (module Evaluator)) list
val remove_recs : string list ref
val add_remove_rec : string -> unit
val remove_rec_all : bool ref
val load_code : unit -> string option
val string_of_tiny : preamble:string -> Tinyocaml.t -> string
val fixup : 'a -> 'b -> 'b
val string_of_op : Ocamliutil.last_op -> string
val show_this_stage :
  Ocamliutil.last_op list ->
  Ocamliutil.last_op list -> Tinyocaml.t -> Tinyocaml.t -> bool
val show_this_pervasive_stage : Ocamliutil.last_op list -> bool
val skipped : bool ref
val wait_for_enter : unit -> unit
val print_string : string -> unit
val eval : string -> string
val eval_ast : Parsetree.structure -> Parsetree.expression
val eval_string : string -> Tinyocaml.t
val eval_string_to_ast : string -> Parsetree.expression

