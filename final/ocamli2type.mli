type op = Add | Sub | Mul | Div

type t' =
  Value of Obj.t
| Var of string
| ArrayExpr of t array (* Array not yet a value e.g [|1 + 2; 3|] *)
| IntOp of op * t * t
| FOp of op * t * t
| ArrayGet of t * t
| ArraySet of t * t * t
| Let of binding * t

and t =
  {typ : Types.type_desc;
   e : t';
   lets : binding list}

and binding = string * t

val is_value : t -> bool

val names_in : t -> string list

val names_in_t' : t' -> string list

val string_of_t : t -> string

val tinyocaml_of_ocaml_heap_value : Types.type_desc -> Obj.t -> Tinyocaml.t

val find_type_desc : Types.type_expr -> Types.type_desc
