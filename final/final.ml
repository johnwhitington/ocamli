open Tinyocaml

(* Now, to build a Tinyocaml.t from a Real OCaml one, we must use a C function
to build the pointer to the float *)

external build'a : 'a -> 'b = "magic"

external from'a : 'a -> 'b = "magic"

let is_value_t' = function
  Value _ -> true
| ArrayExpr _ | IntOp _ | FOp _ | ArrayGet _ | ArraySet _ -> false

let is_value {e} = is_value_t' e

let string_of_op = function
    Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div" 

let rec string_of_tinyocaml_t' = function
  Value _ -> "<value>"
| ArrayExpr items ->
    Printf.sprintf "[|%s|]" (string_of_items (Array.to_list items)) 
| FOp (op, a, b) ->
    Printf.sprintf
      "FOp (%s, %s, %s)"
      (string_of_op op) (string_of_tinyocaml a) (string_of_tinyocaml b)
| IntOp (op, a, b) ->
    Printf.sprintf
      "IntOp (%s, %s, %s)"
      (string_of_op op) (string_of_tinyocaml a) (string_of_tinyocaml b)
| ArrayGet (arr, i) ->
    Printf.sprintf "ArrayGet (%s, %s)" (string_of_tinyocaml arr) (string_of_tinyocaml i)
| ArraySet (arr, i, newval) ->
    Printf.sprintf
      "ArraySet (%s, %s, %s)"
      (string_of_tinyocaml arr) (string_of_tinyocaml i) (string_of_tinyocaml newval)

and string_of_items items =
  List.fold_left ( ^ ) "" (List.map string_of_tinyocaml items)

and string_of_tinyocaml {e} =
  string_of_tinyocaml_t' e

(* Now, the evaluator *)
let perform_float_op op x y =
  match op with
    Add -> x +. y
  | Sub -> x -. y
  | Mul -> x *. y
  | Div -> x *. y

let perform_int_op op x y =
  match op with
    Add -> x +. y
  | Sub -> x -. y
  | Mul -> x *. y
  | Div -> x *. y

(* If an ArrayExpr contains only things which are values, we need to identify
 * it and turn it into a heap object. However, it cannot be considered really a
 * "value", or it would never get evaluted. E.g [|1 + 2; 3|] is an ArrayExpr,
 * but not (yet) a value *)
let rec array_expr_should_be_value arr =
  Array.for_all
    (function {e = ArrayExpr a} -> array_expr_should_be_value a
            | x -> is_value x)
    arr

let rec eval (expr : 'a t) =
  match expr.e with
  FOp (op, {e = Value x}, {e = Value y}) ->
    {expr with e = Value (build'a (perform_float_op op (from'a x) (from'a y)))}
| FOp (op, ({e = Value _} as x), y) ->
    {expr with e = FOp (op, x, eval y)}
| FOp (op, x, y) ->
    {expr with e = FOp (op, eval x, y)}
| ArrayExpr a ->
    if array_expr_should_be_value a then
      {expr with e = Value (External.to_ocaml_value (ArrayExpr a))}
    else 
      begin
        if eval_first_non_value_element a
          then {expr with e = ArrayExpr a}
          else assert false
      end
| ArrayGet (arr, i) ->
    if is_value arr then
      match arr, i with
        {e = Value array_val}, {e = Value index} ->
          {expr with e = Value ((Obj.magic array_val : 'a array).(index))}
      | _ -> {expr with e = ArrayGet (arr, eval i)}
    else
      {expr with e = ArrayGet (eval arr, i)}
| ArraySet (arr, i, e) ->
    if not (is_value arr) then {expr with e = ArraySet (eval arr, i, e)}
    else if not (is_value i) then {expr with e = ArraySet (arr, eval i, e)}
    else if not (is_value e) then {expr with e = ArraySet (arr, i, eval e)}
    else
      begin match arr, i, e with
      | {e = Value array_val}, {e = Value i}, {e = Value newval} ->
          (Obj.magic array_val : 'a array).(i) <- newval;
          {expr with e = Value 1} (* unit *)
      | _ -> assert false
      end
| Value _ -> failwith "already a value"
| _ -> failwith "unimplemented or a type error"

and eval_first_non_value_element arr =
  try
    for x = 0 to Array.length arr - 1 do
      match arr.(x) with
        {e = ArrayExpr arr'} ->
          if eval_first_non_value_element arr' then raise Exit
      | elt ->
          if not (is_value elt) then (arr.(x) <- eval elt; raise Exit)
    done;
    false
  with
    Exit -> true

let example : 'a t =
  {e =
    FOp (Add,
         {e = Value (build'a 1); typ = "int"},
         {e = Value (build'a 2); typ = "int"});
   typ = "int"}

let rec eval_full v =
  Printf.printf "%s\n" (string_of_tinyocaml v);
  if is_value v then v else eval_full (eval v)

let _ =
  match eval_full example with
    {e = Value x} -> Printf.printf "Answer is %f\n" (from'a x)
  | _ -> failwith "answer not a float"

