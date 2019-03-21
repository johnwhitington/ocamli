val eval_full : Type.env ref -> string -> Obj.t

val addenv : Type.env ref -> string option -> string -> Obj.t -> string -> unit

val init : unit -> unit

