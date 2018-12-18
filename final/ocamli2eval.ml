open Ocamli2type

(* Now, the evaluator *)
let perform_float_op op x y =
  match op with
    Add -> x +. y
  | Sub -> x -. y
  | Mul -> x *. y
  | Div -> x /. y

let perform_int_op op x y =
  match op with
    Add -> x + y
  | Sub -> x - y
  | Mul -> x * y
  | Div -> x / y

(* Append two lists which are 'a values *)
let append_lists a b =
  match a, b with
    Value a, Value b ->
      Value (Obj.magic ((Obj.magic a : 'a list) @ (Obj.magic b : 'a list)) : Obj.t)
  | _ -> assert false

(* Pattern matching. FIXME guard etc. *)
let patmatch expr (pat, guard, rhs) =
  let yes = Some rhs and no = None in
  match expr, pat with
    _, PatAny -> yes
  | {e = Value v}, PatConstr ("[]", _) ->
      begin match tinyocaml_of_ocaml_heap_value expr.typ v with
        Tinyocaml.Nil -> yes
      | _ -> no
      end
  | _, _ -> no

let rec eval env expr =
  let env = List.rev expr.lets @ env in
  match expr.e with
  FOp (op, {e = Value x}, {e = Value y}) ->
    {expr with e = Value (Obj.repr (perform_float_op op (Obj.magic x : float) (Obj.magic y : float)))}
| FOp (op, ({e = Value _} as x), y) ->
    {expr with e = FOp (op, x, eval env y)}
| FOp (op, x, y) ->
    {expr with e = FOp (op, eval env x, y)}
| IntOp (op, {e = Value x}, {e = Value y}) ->
    {expr with e = Value (Obj.repr (perform_int_op op (Obj.magic x : int) (Obj.magic y : int)))}
| IntOp (op, ({e = Value _} as x), y) ->
    {expr with e = IntOp (op, x, eval env y)}
| IntOp (op, x, y) ->
    {expr with e = IntOp (op, eval env x, y)}
| Var x ->
    List.assoc x env
| Let ((n, e), e') ->
    let evalled = eval env e in
      if is_value evalled
        then {e = e'.e; typ = e.typ; lets = expr.lets @ [(n, evalled)] @ e'.lets}
        else {expr with e = Let ((n, evalled), e')}
| ArrayExpr a ->
    let evalled =
      if eval_first_non_value_element env a
        then {expr with e = ArrayExpr a}
        else assert false
    in
      if should_be_value evalled then
        {expr with e = Value (Ocamli2read.to_ocaml_heap_value evalled.e)}
      else
        evalled
| Cons (h, t) ->
    let evalled =
      if is_value h then Cons (h, eval env t) else Cons (eval env h, t)
    in
      (*Printf.printf "Evalled is %s\n" (string_of_t' expr.typ evalled);*)
      if should_be_value_t' evalled
        then {expr with e = Value (Ocamli2read.to_ocaml_heap_value evalled)}
        else {expr with e = evalled}
| Append (a, b) when is_value a && is_value b ->
    {expr with e = append_lists a.e b.e}
| Append (a, b) ->
    {expr with e =
      if is_value a then Append (a, eval env b) else Append (eval env a, b)}
| ArrayGet (arr, i) ->
    if is_value arr then
      match arr, i with
        {e = Value array_val}, {e = Value index} ->
          {expr with e = Value ((Obj.magic array_val : 'a array).((Obj.magic index : int)))}
      | _ -> {expr with e = ArrayGet (arr, eval env i)}
    else
      {expr with e = ArrayGet (eval env arr, i)}
| ArraySet (arr, i, e) ->
    if not (is_value arr) then {expr with e = ArraySet (eval env arr, i, e)}
    else if not (is_value i) then {expr with e = ArraySet (arr, eval env i, e)}
    else if not (is_value e) then {expr with e = ArraySet (arr, i, eval env e)}
    else
      begin match arr, i, e with
      | {e = Value array_val}, {e = Value i}, {e = Value newval} ->
          (Obj.magic array_val : 'a array).((Obj.magic i : int)) <- newval;
          {expr with e = Value (Obj.repr ())}
      | _ -> assert false
      end
| Match (e, h::t) ->
    if is_value e then
      begin match patmatch e h with
        Some rhs -> {expr with e = rhs.e}
      | None -> {expr with e = Match (e, t)}
      end
    else
      {expr with e = Match (eval env e, h::t)}
| Match (_, []) ->
    failwith "Matched no pattern"
| Value _ -> failwith "already a value"

and eval_first_non_value_element env arr =
  try
    for x = 0 to Array.length arr - 1 do
      match arr.(x) with
        {e = ArrayExpr arr'} ->
          if eval_first_non_value_element env arr' then raise Exit
      | elt ->
          if not (is_value elt) then (arr.(x) <- eval env elt; raise Exit)
    done;
    false
  with
    Exit -> true
