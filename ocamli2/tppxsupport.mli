val eval_full : Type.env ref -> string -> Obj.t

val eval_full_from_typedtree : Type.env ref -> string -> Obj.t

val addenv : Type.env ref -> string option -> string -> Obj.t -> string -> unit

val init : unit -> unit

