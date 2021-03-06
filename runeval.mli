val show : bool ref
val showall : bool ref
val top : bool ref
val debug : bool ref
val showpervasives : bool ref
val printer : string ref
val width : int ref
val show_simple_arithmetic : bool ref
val debugtiny : bool ref
val debugtinyall : bool ref
val debugpp : bool ref
val prompt : bool ref
val step : float ref
val fastcurry : bool ref
val noifbool : bool ref
val novarlookup : bool ref

type mode = FromFile of string | FromText of string

val source : (string * mode) list ref
val setfile : string -> unit
val settext : ?modname:string -> string -> unit

val remove_recs : string list ref
val add_remove_rec : string -> unit
val remove_rec_all : bool ref

val load_code : unit -> string option
val string_of_tiny : preamble:string -> ?codes:bool -> Tinyocaml.t -> string
val string_of_op : Ocamliutil.last_op -> string

val show_this_stage :
  Ocamliutil.last_op list ->
  Ocamliutil.last_op list -> Tinyocaml.t -> Tinyocaml.t -> bool

val show_this_pervasive_stage : Ocamliutil.last_op list -> bool

val skipped : bool ref
val wait_for_enter : Eval.t -> string option

val print_string : string -> unit

val eval_string_to_ast : ?stdlib:bool -> ?filename:string -> string -> Parsetree.expression

val eval_string :  ?stdlib:bool -> ?filename:string -> string -> string
