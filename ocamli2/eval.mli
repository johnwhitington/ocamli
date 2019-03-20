val showrules : bool ref
val print : bool ref
val showsteps : bool ref
val peek : bool ref

val eval_full : Type.env -> Type.t -> Type.t

val make_native : Type.env -> Type.t -> Obj.t

