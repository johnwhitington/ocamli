type op = Add | Sub | Mul | Div

type binding = string * t

and envitem = bool * binding list ref

and environment = envitem list

and annotation = string

and t =
  {t : t';
   lets : (bool * binding list) list; (* The implicit value-lets around any expression *)
   annots : annotation list;
  }

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

val of_tinyocaml : Tinyocaml.t -> t

val eval : environment -> t -> t

