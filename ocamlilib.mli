(* ocamlilib *)
val debug : bool ref
val otherlibs : string ref

val showlib : unit -> unit

val load_stdlib : bool ref

val load_library : unit -> unit

val showstdlibinit : bool ref

val load_library_modules : (string * string) list -> Tinyocaml.env

