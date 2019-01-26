val to_ocaml_heap_value : Ocamli2type.t' -> Obj.t

val finaltype_of_typedtree : Typedtree.structure -> Ocamli2type.t

val typedtree_of_string : ?filename:string -> string -> Typedtree.structure

