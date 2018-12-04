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
    if Ocamli2read.array_expr_should_be_value a then
      {expr with e = Value (Ocamli2read.to_ocaml_heap_value (ArrayExpr a))}
    else 
      begin
        if eval_first_non_value_element env a
          then {expr with e = ArrayExpr a}
          else assert false
      end
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
