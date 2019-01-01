open Ocamli2type

let show_all_lets = ref false

(* For now, convert to tinyocaml thence to pptinyocaml. Soon, we will need our own prettyprinter, of course *)
let tinyocaml_op_of_finaltype_op = function
  Add -> Tinyocaml.Add
| Sub -> Tinyocaml.Sub
| Mul -> Tinyocaml.Mul
| Div -> Tinyocaml.Div

let rec tinyocaml_of_finaltype_t' typ = function
  Value x -> tinyocaml_of_ocaml_heap_value typ x
| Function (cases, env) -> Tinyocaml.Function (List.map tinyocaml_of_finaltype_case cases, [])
| Apply (e, [e']) -> Tinyocaml.App (tinyocaml_of_finaltype e, tinyocaml_of_finaltype e')
| Apply (e, [e'; e'']) ->
    Tinyocaml.App (Tinyocaml.App (tinyocaml_of_finaltype e, tinyocaml_of_finaltype e'), tinyocaml_of_finaltype e'')
| Apply _ -> failwith "unknown Apply"
| Var x -> Tinyocaml.Var x
| ArrayExpr arr -> Tinyocaml.Array (Array.map tinyocaml_of_finaltype arr)
| Cons (h, t) -> Tinyocaml.Cons (tinyocaml_of_finaltype h, tinyocaml_of_finaltype t)
| Append (a, b) -> Tinyocaml.Append (tinyocaml_of_finaltype a, tinyocaml_of_finaltype b)
| IntOp (op, x, y) ->
    Tinyocaml.Op
      (tinyocaml_op_of_finaltype_op op,
       tinyocaml_of_finaltype x,
       tinyocaml_of_finaltype y)
| FOp (op, x, y) ->
    Tinyocaml.App
      ((Tinyocaml.App (Var "Stdlib.+.", tinyocaml_of_finaltype x)), tinyocaml_of_finaltype y)
| ArrayGet (x, y) ->
    Tinyocaml.App
      ((Tinyocaml.App
        (Var "Stdlib.Array.get", tinyocaml_of_finaltype x)),
      (tinyocaml_of_finaltype y))
| ArraySet (arr, index, newval) ->
    Tinyocaml.App
      (Tinyocaml.App
        ((Tinyocaml.App
          (Var "Stdlib.Array.set", tinyocaml_of_finaltype arr)),
        (tinyocaml_of_finaltype index)),
        (tinyocaml_of_finaltype newval))
| Let (recflag, (n, a), b) ->
    Tinyocaml.Let
      (recflag,
       [(Tinyocaml.PatVar n, tinyocaml_of_finaltype a)],
       tinyocaml_of_finaltype b)
| Match (e, cases) ->
    Tinyocaml.Match
      (tinyocaml_of_finaltype e,
       List.map tinyocaml_of_finaltype_case cases)
| Struct ls ->
    Tinyocaml.Struct (false, List.map tinyocaml_of_finaltype ls)
| LetDef (recflag, (n, e)) ->
    Tinyocaml.LetDef (recflag, [(PatVar n, tinyocaml_of_finaltype e)]) 

and tinyocaml_of_finaltype_case (pat, guard, rhs) =
  (tinyocaml_of_finaltype_pattern pat,
   tinyocaml_of_finaltype_guard guard,
   tinyocaml_of_finaltype rhs)

and tinyocaml_of_finaltype_guard = function
  None -> None
| Some g -> Some (tinyocaml_of_finaltype g)

and tinyocaml_of_finaltype_pattern = function
  PatAny -> Tinyocaml.PatAny
| PatConstr ("[]", []) -> Tinyocaml.PatNil
| PatVar v -> Tinyocaml.PatVar v
| PatConstr ("::", [h; t]) ->
    Tinyocaml.PatCons (tinyocaml_of_finaltype_pattern h, tinyocaml_of_finaltype_pattern t)
| PatConstant (IntConstant i) ->
    Tinyocaml.PatInt i
| _ -> failwith "tinyocaml_of_finaltype_pattern: unknown"

(* FIXME Need to remove anything shadowed by a name binding because of a pattern in a pattern match too *)
  (* If any implicit lets, fabricate them -- but only if they are used in the
   * expression underneath, and not shadowed. *)
  (*Printf.printf "We have %i lets\n" (List.length lets);*)
and tinyocaml_of_finaltype {e; typ; lets} =
  let remove_names_from_lets names =
    (* Remove any name in [names] from any let in the implicit lets, removing
     * any let-binding which is now empty *)
    List.filter (fun (v, _) -> List.mem v names)
  in
  let rec remove_shadowed_implicits = function
    [] -> []
  | (n, e)::r ->
      if List.mem n (List.map fst r)
        then remove_shadowed_implicits r
        else (n, e)::remove_shadowed_implicits r
  in
  let rec fabricate_lets e = function
    [] -> e
  | (n, rhs)::r ->
      fabricate_lets (Tinyocaml.Let (false, [(Tinyocaml.PatVar n, tinyocaml_of_finaltype rhs)], e)) r
  in
  let inner = tinyocaml_of_finaltype_t' typ e in
    if lets = [] then inner else
      let names = names_in_t' e in
      (*Printf.printf "%i names in t'\n" (List.length names);*)
      let lets_to_print =
        if !show_all_lets then lets else
          remove_names_from_lets names (remove_shadowed_implicits lets)
      in
        (*Printf.printf "lets to print: %i\n" (List.length lets_to_print);*)
        fabricate_lets inner (List.rev lets_to_print)

