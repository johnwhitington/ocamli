val eval_full : Type.env ref -> string -> Obj.t

val addenv : (bool * ('a * Type.t) list ref) list ref -> 'a -> Obj.t -> string -> unit

