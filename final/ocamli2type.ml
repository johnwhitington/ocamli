open Types


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
| Let of bool * binding * t
| Match of t * case list
| Struct of t list
| LetDef of bool * binding

and t =
  {typ : Types.type_desc;
   e : t';
   lets : env}

and env = envitem list

and envitem = bool * binding list ref

and binding = string * t

and case = pattern * t option * t

let rec is_value_t' = function
  Value _ | Function _ -> true
| ArrayExpr _ | Append _ | Cons _ | IntOp _ | FOp _
| ArrayGet _ | ArraySet _ | Let _ | Var _ | Match _ | Apply _ -> false
| Struct l -> List.for_all is_value l
| LetDef (_, (_, e)) -> is_value e

and is_value {e} = is_value_t' e

let rec should_be_value_t' = function
  x when is_value_t' x -> true
| ArrayExpr arr -> Array.for_all should_be_value arr
| Cons (h, t) -> should_be_value h && should_be_value t
| _ -> false

and should_be_value {e} = should_be_value_t' e

(* List the names in an expression, including any implicit let-bindings. *)
let rec names_in_t' = function
  Value _ -> []
| Function (cases, env) -> List.flatten (List.map names_in_case cases)
| Apply (a, es) -> names_in a @ List.flatten (List.map names_in es)
| Var x -> [x]
| ArrayExpr elts -> List.flatten (Array.to_list (Array.map names_in elts))
| IntOp (_, e, e') | FOp (_, e, e') | ArrayGet (e, e') | Cons (e, e') | Append (e, e') ->
    names_in e @ names_in e'
| Let (_, binding, e) -> names_in_binding binding @ names_in e
| ArraySet (e, e', e'') -> names_in e @ names_in e' @ names_in e''
| Match (e, cases) -> names_in e @ List.flatten (List.map names_in_case cases)
| Struct l -> List.flatten (List.map names_in l)
| LetDef (_, (n, e)) -> n :: names_in e

(* FIXME What to do here? Are we supposed to count the pattern name and all
 * uses and what about shadows? The only use of this function at the moment is
 * in the writer for not showing occluded lets. So combine it with that or
 * specify it properly... *)
and names_in_case (pat, guard, e) =
  names_in e @
  begin match guard with None -> [] | Some e -> names_in e end

and names_in_binding (n, e) =
  n :: names_in e 

and names_in_envitem (_, r) =
  List.flatten (List.map names_in_binding !r)

and names_in {lets; e} =
  List.flatten (List.map names_in_envitem lets) @ names_in_t' e


let rec find_type_desc {desc} =
  match desc with
    Tlink x -> find_type_desc x
  | typ -> typ

