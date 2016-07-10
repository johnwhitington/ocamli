
(*let bound_in_environment_item (_, bindings) =
  List.flatten (List.map (fun (p, _) -> bound_in_pattern p) bindings)

let bound_in_environment env =
  List.flatten (List.map bound_in_environment_item env)

(* List the identifiers used in an expression which are not defined in it. *)
let rec free (bound : string list) (expr : t) =
  match expr with
  | Var s -> if List.mem s bound then [] else [s]
  (* Things which can bind names (and may contain subexpressions too) *)
  | Let (recflag, bindings, e) ->
      free_in_bindings bound recflag bindings e
  | LetDef (recflag, bindings) ->
      free_in_bindings bound recflag bindings Unit
  | Fun (pattern, e, _) ->
      free (bound_in_pattern pattern @ bound) e
  | Function (cases, _) ->
      free_in_cases bound cases
  | For (name, e, _, e', e'', _) ->
      free bound e @ free bound e' @ free (name::bound) e''
  | Match (e, cases) ->
      free bound e @ free_in_cases bound cases
  (* Other things which contain subexpressions *)
  | Record items ->
      List.fold_left ( @ ) []
        (List.map (free bound) (List.map (fun (_, {contents}) -> contents) items))
  | Struct (_, es)
  | Tuple es
  | Sig es ->
      List.fold_left ( @ ) [] (List.map (free bound) es)
  | Array es ->
      Array.fold_left ( @ ) [] (Array.map (free bound) es)
  | Raise (_, Some e)
  | Assert e
  | Field (e, _)
  | TryWith (e, _)
  | Control (_, e) ->
      free bound e
  | Op (_, e, e')
  | Cmp (_, e, e')
  | Append (e, e')
  | While (e, e', _, _)
  | SetField (e, _, e')
  | App (e, e')
  | Seq (e, e') ->
      free bound e @ free bound e'
  | If (e, e', e'') ->
      free bound e @ free bound e' @
      (match e'' with None -> [] | Some e'' -> free bound e'')
  (* All others *)
  | x -> [] (* FIXME add cons, constr, any others. *)

(* A variable is free in a case if it is free in the guard or rhs *)
and free_in_case bound (pat, guard, rhs) =
  let bound = bound_in_pattern pat @ bound in
      free bound rhs @ (match guard with None -> [] | Some g -> free bound g)

and free_in_cases bound cases =
  List.flatten (List.map (free_in_case bound) cases)

and free_in_binding bound (pat, t) =
  free bound t

and free_in_bindings bound recflag bindings e =
  let bound' =
    List.flatten (List.map (fun (p, _) -> bound_in_pattern p) bindings)
  in
     List.flatten
       (List.map (free_in_binding (if recflag then bound' else bound)) bindings)
   @ free bound' e

let free = free []

(* Given a list of variables free in some code, and the current environment,
produce a new environment containing just those ones which are free.
Duplicates and the order are retained *)
let any_var_in_bindings free ((_, bindings) as envitem) =
  if
    List.exists
      (fun x -> List.mem x free)
      (List.flatten (List.map (fun (p, _) -> bound_in_pattern p) bindings))
  then
    Some envitem
  else
    None

let prune_environment (free : string list) (env : env) : env =
  Ocamliutil.option_map (any_var_in_bindings free) env*)

