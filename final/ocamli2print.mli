(* FIXME put this back afterward *)
(*val print : ?preamble:string -> Format.formatter -> Ocamli2type.t -> unit*)

val string_of_t : Ocamli2type.t -> string

val string_of_t' : Types.type_desc -> Ocamli2type.t' -> string

(* Will use tinyocaml_of_heap_value for now until native *)
val to_string_from_heap : ?preamble:string -> Types.type_desc -> Obj.t -> string

(* Will use tinyocaml_of_finaltype for now until native *)
val to_string_from_finaltype : ?preamble:string -> Ocamli2type.t -> string

(* Will go away when pattern matching fixed to no longer need Tinyocaml *)
val tinyocaml_of_ocaml_heap_value : Types.type_desc -> Obj.t -> Tinyocaml.t

val syntax : bool ref

val syntax_tex : bool ref

val simple : bool ref

val width : int ref

val fastcurry : bool ref

val debug : bool ref

val show_all_lets : bool ref

val showvals : bool ref
