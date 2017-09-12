type op = Add | Sub | Mul | Div

type binding = string * t

and envitem = bool * binding list ref

and environment = envitem list

and t = { t : t'; lets : (bool * binding list) list; }

and t' =
    Int of int
  | Bool of bool
  | Var of string
  | If of t * t * t
  | Op of op * t * t
  | Equals of t * t
  | Let of bool * binding list * t
  | LetDef of bool * binding list
  | Apply of t * t
  | Function of string * environment * t
  | Struct of t list

val mkt : t' -> t

val is_value_t' : t' -> bool

val is_value_binding : binding -> bool

val is_value : t -> bool

val string_of_t' : t' -> string

val string_of_implicit_lets : (bool * binding list) list -> string

val string_of_t : t -> string

val string_of_binding : binding -> string

val string_of_bindings : binding list -> string

val string_of_op : op -> string

val print_env_item : bool * binding list ref -> unit

val print_env : environment -> unit

val lookup_in_bindings : 'a -> ('a * 'b) list -> 'b option

val lookup_in_environment : string -> environment -> t

val envitem_of_bindings : 'a -> 'b -> 'a * 'b ref

val add_implicit_lets_to_environment :
  ('a * 'b ref) list -> ('a * 'b) list -> ('a * 'b ref) list
