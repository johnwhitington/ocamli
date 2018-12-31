type op = Add | Sub | Mul | Div

type patconstant =
  IntConstant of int

type pattern =
  PatAny
| PatVar of string
| PatConstr of string * pattern list
| PatConstant of patconstant

type t' =
  Value of Obj.t
| Function of case list * env
| Apply of t * t list
| Var of string
| ArrayExpr of t array (* Array not yet a value e.g [|1 + 2; 3|] *)
| Cons of t * t (* Cons part of list literal which is not yet a value e.g [1 + 2; 3] *)
| Append of t * t
| IntOp of op * t * t
| FOp of op * t * t
| ArrayGet of t * t
| ArraySet of t * t * t
| Let of bool * binding * t (* recflag *)
| Match of t * case list
| Struct of t list
| LetDef of bool * binding (* recflag *)

and t =
  {typ : Types.type_desc;
   e : t';
   lets : binding list}

and env = envitem list

and envitem = bool * binding list ref

and binding = string * t

and case = pattern * t option * t

val is_value : t -> bool

val should_be_value : t -> bool

val should_be_value_t' : t' -> bool

val names_in : t -> string list

val names_in_t' : t' -> string list

val string_of_t : t -> string

val string_of_t' : Types.type_desc -> t' -> string

val tinyocaml_of_ocaml_heap_value : Types.type_desc -> Obj.t -> Tinyocaml.t

val find_type_desc : Types.type_expr -> Types.type_desc
