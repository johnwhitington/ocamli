val to_ocaml_heap_value : Type.t -> Obj.t

val finaltype_of_typedtree : Typedtree.structure -> Type.t

val finaltype_of_expression : Type.env -> Typedtree.expression -> Type.t

val typedtree_of_string : ?filename:string -> string -> Typedtree.structure

val remove_links : Types.type_expr -> Types.type_expr

val debug_type : Types.type_expr -> Types.type_expr

val read : string -> Type.t 
