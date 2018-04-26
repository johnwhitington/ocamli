open Tinyocaml

(*FIXME Add ints. Floatarray is an unneeded complication for now. *)

(* Now, to build a Tinyocaml.t from a Real OCaml one, we must use a C function
to build the pointer to the float *)

external build'a : 'a -> 'b = "magic"

external from'a : 'a -> 'b = "magic"

let is_value = function
  Unit | Int _ | Bool _ | Value _ -> true
| ArrayExpr _ | FOp _ | ArrayGet _ | ArraySet _ -> false

let string_of_op = function
    Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div" 

let rec string_of_tinyocaml = function
  Unit -> "Unit"
| Int i -> Printf.sprintf "Int %i" i
| Bool b -> Printf.sprintf "Bool %b" b
| Value _ -> "<heap>"
| ArrayExpr items ->
    Printf.sprintf "[|%s|]" (string_of_items (Array.to_list items)) 
| FOp (op, a, b) ->
    Printf.sprintf
      "FOp (%s, %s, %s)"
      (string_of_op op) (string_of_tinyocaml a) (string_of_tinyocaml b)
| ArrayGet (arr, i) ->
    Printf.sprintf "ArrayGet (%s, %s)" (string_of_tinyocaml arr) (string_of_tinyocaml i)
| ArraySet (arr, i, newval) ->
    Printf.sprintf
      "ArraySet (%s, %s, %s)"
      (string_of_tinyocaml arr) (string_of_tinyocaml i) (string_of_tinyocaml newval)

and string_of_items items =
  List.fold_left ( ^ ) "" (List.map string_of_tinyocaml items)

(* Now, the evaluator *)
let perform_op op x y =
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
    (function ArrayExpr a -> array_expr_should_be_value a
            | x -> is_value x)
    arr

let rec eval = function
  FOp (op, Value x, Value y) ->
    Value (build'a (perform_op op (from'a x) (from'a y)))
| FOp (op, Value x, y) -> FOp (op, Value x, eval y)
| FOp (op, x, y) -> FOp (op, eval x, y)
| ArrayExpr a ->
    if array_expr_should_be_value a then
      Value (External.to_ocaml_value (ArrayExpr a))
    else 
      begin
        if eval_first_non_value_element a
          then ArrayExpr a
          else assert false
      end
| ArrayGet (arr, i) ->
    if is_value arr then
      match arr, i with
        Value array_val, Int index ->
          Value ((Obj.magic array_val : 'a array).(index))
      | _ -> ArrayGet (arr, eval i)
    else
      ArrayGet (eval arr, i)
| ArraySet (arr, i, e) ->
    if not (is_value arr) then ArraySet (eval arr, i, e)
    else if not (is_value i) then ArraySet (arr, eval i, e)
    else if not (is_value e) then ArraySet (arr, i, eval e)
    else
      begin match arr, i, e with
      | Value array_val, Int i, Value newval ->
          (Obj.magic array_val : 'a array).(i) <- newval;
          Unit
      | _ -> assert false
      end
| Int _ | Bool _ | Value _ -> failwith "already a value"
| _ -> failwith "unimplemented or a type error"

and eval_first_non_value_element arr =
  try
    for x = 0 to Array.length arr - 1 do
      match arr.(x) with
        ArrayExpr arr' ->
          if eval_first_non_value_element arr' then raise Exit
      | elt ->
          if not (is_value elt) then (arr.(x) <- eval elt; raise Exit)
    done;
    false
  with
    Exit -> true

(* The "unit" here is to prevent "...contains type variables which cannot be
 * generalized." *)
let example : unit t =
  FOp (Add, Value (build'a 1.), Value (build'a 2.))

let rec eval_full v =
  Printf.printf "%s\n" (string_of_tinyocaml v);
  if is_value v then v else eval_full (eval v)

let _ =
  match eval_full example with
    Value x -> Printf.printf "Answer is %f\n" (from'a x)
  | _ -> failwith "answer not a float"

