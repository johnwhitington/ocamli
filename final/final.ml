(* This type has a type variable to allow us to represent pointers into the heap *)
type op = Add | Sub | Mul | Div

type 'a t =
(* values *)
  Int of int
| Bool of bool
| Float of 'a (* Floats are boxed. *)
| Array of 'a
(* non-values *)
| FOp of (op * 'a t * 'a t)
| ArrayGet of ('a t * int)
| ArraySet of ('a t * int * 'a t)

(* Now, to build a Tinyocaml.t from a Real OCaml one, we must use a C function to build the pointer to the float *)

external build'a : 'a -> 'b = "magic"

external from'a : 'a -> 'b = "magic"

let is_value = function
  Int _ | Bool _ | Float _ -> true
| Array _ -> true (* fixme need to check all its elts are values *)
| FOp _ | ArrayGet _ | ArraySet _ -> false

(* Now, the evaluator *)
let perform_op op x y =
  match op with
    Add -> x +. y
  | Sub -> x -. y
  | Mul -> x *. y
  | Div -> x *. y

let rec eval = function
  FOp (op, Float x, Float y) ->
    Float (build'a (perform_op op (from'a x) (from'a y)))
| FOp (op, Float x, y) -> FOp (op, Float x, eval y)
| FOp (op, x, y) -> FOp (op, eval x, y)
(*| ArrayGet (arr, i) ->
    (* We don't know the type, so what data can we make? *)
| ArraySet (arr, i, newval) ->
| Array x -> (* not all its elts are values *)*)
| _ -> failwith "Already a value or malformed"

let example =
  FOp (Add, Float (build'a 1.), Float (build'a 2.))

let rec eval_full v =
  if is_value v then v else eval_full (eval v)

let _ =
  match eval_full example with
    Float x -> Printf.printf "Answer is %f\n" (from'a x)
  | _ -> failwith "answer not a float"

