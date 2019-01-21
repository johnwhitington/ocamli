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

(* Map over the data structure, given a function from t -> t *)
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

and map_t f t = f t

and map_env f env = List.map (map_envitem f) env

and map_envitem f (recflag, {contents}) =
  (recflag, {contents = List.map (map_binding f) contents})

and map_binding f (n, e) = (n, map_t f e)

and map_case f = function
  (p, None, rhs) -> (p, None, map_t f rhs)
| (p, Some guard, rhs) -> (p, Some (map_t f guard), map_t f rhs)

(* List of the names which are free in an expression (may contain duplicates). *)
let remove_name n = List.filter (fun x -> x <> n)

let rec free_in_t' = function
  Value _ -> []
| Function (cases, _) -> free_in_cases cases
| Apply (a, es) -> free_in a @ List.flatten (List.map free_in es)
| Var x -> [x]
| ArrayExpr elts -> List.flatten (Array.to_list (Array.map free_in elts))
| IntOp (_, e, e') | FOp (_, e, e') | ArrayGet (e, e') | Cons (e, e') | Append (e, e') ->
    free_in e @ free_in e'
| Let (recflag, (n, e), e') ->
    let in_e = free_in e in
    let in_e' = free_in e' in
      remove_name n in_e' @ if recflag then remove_name n in_e else in_e
| ArraySet (e, e', e'') -> free_in e @ free_in e' @ free_in e''
| Match (e, cases) -> free_in e @ free_in_cases cases
| Struct l -> free_in_multiple_items l
| LetDef (_, (n, e)) -> failwith "free_in_multiple_items: LetDef"

and free_in_multiple_items = function
  [] -> []
| {e = LetDef (recflag, (n, e))}::t ->
    let below = free_in_multiple_items t in
    let from_e = free_in e in
      remove_name n below @ if recflag then remove_name n from_e else from_e
| _ -> failwith "free_in_multiple_items: unexpected structure item"

and free_in_case (pat, guard, e) =
  let name_of_pattern = function
    | PatVar v -> Some v
    | _ -> None
  in
    let names = 
      free_in e @ (match guard with None -> [] | Some e -> free_in e)
    in
      match name_of_pattern pat with
      | Some name -> remove_name name names
      | None -> names

and free_in_cases cases =
  List.flatten (List.map free_in_case cases)

(* B. What do we do with implicit lets when doing free? Same as explicit lets. FIX. *)
and free_in {e} =
  free_in_t' e

(* Remove unused implicit lets from an expression so it may be printed better.
 * This function works by using [map_t] to map over the expression, beginning
 * at the leaves of the tree and working upward. Thus, at each node, implicit
 * lets may be removed. Then, at the node above, they have already gone, so do
 * not count as used from that node. *)
let remove_lets tokeep lets =
  Ocamli2util.option_map
    (fun x ->
       if List.for_all (fun (n, e) -> not (List.mem n tokeep)) !(snd x)
         then None
         else Some x)
    lets

(* Remove duplicate implicit lets. For example let x = 1 in let x = 2 in ...
 * --> let x = 2 in. Only bothers with single bindings for now. *)
let trim_lets lets = lets

let rec remove_unused_lets t =
  map_t
    (fun t ->
      let t2 =
        {t with e = map_t' remove_unused_lets t.e}
      in
        {t2 with lets = trim_lets (remove_lets (free_in_t' t2.e) t2.lets)})
    t

(* Follow any Tlinks left from typechecking, to make pattern matching on types easier. *)
let rec find_type_desc {desc} =
  match desc with
    Tlink x -> find_type_desc x
  | typ -> typ

