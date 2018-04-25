(* This type has a type variable to allow us to represent pointers into the heap *)
type op = Add | Sub | Mul | Div

type 'a t =
  Int of int
| Bool of bool
| Value of 'a       (* All boxed values e.g floats, arrays. *)
| ArrayExpr of 'a t array (* Array expression which is not a value e.g [|1 + 2; 3|] *)
| FOp of (op * 'a t * 'a t)
| ArrayGet of ('a t * int)
| ArraySet of ('a t * int * 'a t)

(* Now, to build a Tinyocaml.t from a Real OCaml one, we must use a C function
to build the pointer to the float *)

external build'a : 'a -> 'b = "magic"

external from'a : 'a -> 'b = "magic"

let is_value = function
  Int _ | Bool _ | Value _ -> true
| ArrayExpr _ | FOp _ | ArrayGet _ | ArraySet _ -> false

let string_of_op = function
    Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div" 

let rec string_of_tinyocaml = function
  Int i -> Printf.sprintf "Int %i" i
| Bool b -> Printf.sprintf "Bool %b" b
| Value _ -> "<heap>"
| ArrayExpr items ->
    Printf.sprintf "[|%s|]" (string_of_items (Array.to_list items)) 
| FOp (op, a, b) ->
    Printf.sprintf
      "FOp (%s, %s, %s)"
      (string_of_op op) (string_of_tinyocaml a) (string_of_tinyocaml b)
| ArrayGet (arr, i) ->
    Printf.sprintf "ArrayGet (%s, %i)" (string_of_tinyocaml arr) i
| ArraySet (arr, i, newval) ->
    Printf.sprintf "ArraySet (%s, %i, %s)" (string_of_tinyocaml arr) i (string_of_tinyocaml newval)

and string_of_items items =
  List.fold_left ( ^ ) "" (List.map string_of_tinyocaml items)

(* Now, the evaluator *)
let perform_op op x y =
  match op with
    Add -> x +. y
  | Sub -> x -. y
  | Mul -> x *. y
  | Div -> x *. y

let rec eval = function
  FOp (op, Value x, Value y) ->
    Value (build'a (perform_op op (from'a x) (from'a y)))
| FOp (op, Value x, y) -> FOp (op, Value x, eval y)
| FOp (op, x, y) -> FOp (op, eval x, y)
| ArrayExpr a ->
    if array_expr_should_be_value a then
      (* turn into heap object *)
    else 
      ArrayExpr (eval_first_non_value_element a)
(*| ArrayGet (arr, i) ->
    (* Call the array getter and return a Value *)
| ArraySet (arr, i, e) ->
    (* If e needs evaluting do it *)
    (* If e already a value, we call the array setter *)*)
| Int _ | Bool _ | Value _ -> failwith "already a value"
| _ -> failwith "unimplemented or a type error"

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

