type binding = string * t

and bindings = binding list

and t =
  {t : t';
   lets : (bool * bindings) list} (* The implicit value-lets around any expression *)

and t' =
  Int of int
| Bool of bool
| Var of string
| IfThenElse of t * t * t
| Times of t * t
| Minus of t * t
| Equals of t * t
| Let of bool * binding list * t
| Apply of t * t
| Function of string * t 
| Struct of t list

val mkt : t' -> t

val eval : binding list -> t -> t

val factorial : t

