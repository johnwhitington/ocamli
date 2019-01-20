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

(* Check if something is a value *)
let rec is_value_t' = function
  Value _ | Function _ -> true
| ArrayExpr _ | Append _ | Cons _ | IntOp _ | FOp _
| ArrayGet _ | ArraySet _ | Let _ | Var _ | Match _ | Apply _ -> false
| Struct l -> List.for_all is_value l
| LetDef (_, (_, e)) -> is_value e

and is_value {e} = is_value_t' e

(* Check if something should be a value i.e is already a value or is an
 * un-normalised but constant expression which should be converted to a heap
 * value *)
let rec should_be_value_t' = function
  x when is_value_t' x -> true
| ArrayExpr arr -> Array.for_all should_be_value arr
| Cons (h, t) -> should_be_value h && should_be_value t
| _ -> false

and should_be_value {e} = should_be_value_t' e

(* Set of the names which are free in an expression. *)
let rec names_in_t' = function
  Value _ -> []
| Function (cases, env) ->
    (* 1. Fix to exclude bound names *)
    List.flatten (List.map names_in_case cases)
| Apply (a, es) -> names_in a @ List.flatten (List.map names_in es)
| Var x -> [x]
| ArrayExpr elts -> List.flatten (Array.to_list (Array.map names_in elts))
| IntOp (_, e, e') | FOp (_, e, e') | ArrayGet (e, e') | Cons (e, e') | Append (e, e') ->
    names_in e @ names_in e'
| Let (_, binding, e) ->
    (* 2. Fix to exclude bound names Also rec? *)
    names_in_binding binding @ names_in e
| ArraySet (e, e', e'') -> names_in e @ names_in e' @ names_in e''
| Match (e, cases) ->
    (* 3. Fix to exclude bound names *)
    names_in e @ List.flatten (List.map names_in_case cases)
| Struct l ->
    (* 4. Fix to apply lets to what is below *)
    List.flatten (List.map names_in l)
| LetDef (_, (n, e)) ->
    (* 5. Fix to exclude bound names in rec. *)
    n :: names_in e

(* Fix because of (1, 3) *)
and names_in_case (pat, guard, e) =
  names_in e @
  begin match guard with None -> [] | Some e -> names_in e end

(* Fix because of (2) *)
and names_in_binding (n, e) =
  n :: names_in e 

(* Fix because of (7) *)
and names_in_envitem (_, r) =
  List.flatten (List.map names_in_binding !r)

and names_in {lets; e} =
  (* 6. Fix to make it a set *)
  (* 7. Fix to deal with implcit lets properly *)
  List.flatten (List.map names_in_envitem lets) @ names_in_t' e

(* Main procedure. Find names used in any t' and remove them from the t. Do
 * this for every t in the whole expression. This relies on implicit lets not
 * being reported as "used names" in names_in. They are consulted when
 * calculating whether they occlude names under them, though. *)
let rec map_t' f = function
  Value v -> Value v
| Function (cases, env) -> Function (List.map (map_case f) cases, map_env f env)
| Apply (func, args) -> Apply (map_t f func, List.map (map_t f) args)
| Var v -> Var v
| ArrayExpr elts -> ArrayExpr (Array.map (map_t f) elts)
| Cons (h, t) -> Cons (map_t f h, map_t f t)
| Append (a, b) -> Append (map_t f a, map_t f b)
| IntOp (op, x, y) -> IntOp (op, map_t f x, map_t f y)
| FOp (op, x, y) -> FOp (op, map_t f x, map_t f y)
| ArrayGet (a, b) -> ArrayGet (map_t f a, map_t f b)
| ArraySet (a, b, c) -> ArraySet (map_t f a, map_t f b, map_t f c)
| Let (recflag, binding, e) -> Let (recflag, map_binding f binding, map_t f e)
| Match (e, cases) -> Match (map_t f e, List.map (map_case f) cases)
| Struct items -> Struct (List.map (map_t f) items)
| LetDef (recflag, binding) -> LetDef (recflag, map_binding f binding)

and map_t f t = {t with e = map_t' f t.e; lets = map_env f t.lets}

and map_env f env = List.map (map_envitem f) env

and map_envitem f (recflag, {contents}) =
  (recflag, {contents = List.map (map_binding f) contents})

and map_binding f (n, e) = (n, map_t f e)

and map_case f = function
  (p, None, rhs) -> (p, None, map_t f rhs)
| (p, Some guard, rhs) -> (p, Some (map_t f guard), map_t f rhs)

(* Remove unused implicit lets from an expression so it may be printed better.
 * This function works by using [map_t] to map over the expression, beginning
 * at the leaves of the tree and working upward. Thus, at each node, implicit
 * lets may be removed. Then, at the node above, they have already gone, so do
 * not count as used from that node. *)
let remove_unused_lets t =
  map_t (fun x -> x) t

(* Follow any Tlinks left from typechecking, to make pattern matching on types easier. *)
let rec find_type_desc {desc} =
  match desc with
    Tlink x -> find_type_desc x
  | typ -> typ

