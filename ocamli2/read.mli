val to_ocaml_heap_value : Type.t -> Obj.t

val finaltype_of_typedtree : Typedtree.structure -> Type.t

val finaltype_of_expression : Type.env -> Typedtree.expression -> Type.t

val typedtree_of_string : ?filename:string -> string -> Typedtree.structure

val read : string -> Type.t 
