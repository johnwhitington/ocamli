open Types

type op = Add | Sub | Mul | Div

type t' =
  Value of Obj.t
| Var of string
| ArrayExpr of t array (* Array not yet a value e.g [|1 + 2; 3|] *)
| IntOp of op * t * t
| FOp of op * t * t
| ArrayGet of t * t
| ArraySet of t * t * t
| Let of binding * t

and t =
  {typ : Types.type_desc;
   e : t';
   lets : binding list}

and binding = string * t

let is_value_t' = function
  Value _ -> true
| ArrayExpr _ | IntOp _ | FOp _ | ArrayGet _ | ArraySet _ | Let _ | Var _  -> false

let is_value {e} = is_value_t' e

(* List the names in an expression, including any implicit let-bindings. *)
let rec names_in_t' = function
  Value _ -> []
| Var x -> [x]
| ArrayExpr elts -> List.flatten (Array.to_list (Array.map names_in elts))
| IntOp (_, e, e') | FOp (_, e, e') | ArrayGet (e, e') -> names_in e @ names_in e'
| Let (binding, e) -> names_in_binding binding @ names_in e
| ArraySet (e, e', e'') -> names_in e @ names_in e' @ names_in e''

and names_in_binding (n, e) =
  n :: names_in e 

and names_in {lets; e} =
  List.flatten (List.map names_in_binding lets) @ names_in_t' e

let string_of_op = function
    Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div" 

let rec find_type_desc {desc} =
  match desc with
    Tlink x -> find_type_desc x
  | typ -> typ

let rec tinyocaml_of_ocaml_heap_value (typ : type_desc) (value : Obj.t) =
  (*Printf.printf "tinyocaml_of_ocaml_heap_value: %s\n" (string_of_ocaml_type typ);*)
  match typ with
    Tconstr (p, _, _) when Path.name p = "int" -> Tinyocaml.Int (Obj.magic value : int)
  | Tconstr (p, _, _) when Path.name p = "float" -> Tinyocaml.Float (Obj.magic value : float)
  | Tconstr (p, _, _) when Path.name p = "unit" -> Tinyocaml.Unit
  | Tconstr (p, [elt_t], _) when Path.name p = "array" ->
      Tinyocaml.Array
        (Array.init
          (Obj.size value)
          (fun i -> tinyocaml_of_ocaml_heap_value (find_type_desc elt_t) (Obj.field value i)))
  | _ -> failwith "tinyocaml_of_ocaml_heap_value: unknown type"

let rec string_of_t' typ = function
  Value x -> Pptinyocaml.to_string (tinyocaml_of_ocaml_heap_value typ x)
| Var x -> Printf.sprintf "Var %s" x
| ArrayExpr items ->
    Printf.sprintf "[|%s|]" (string_of_items (Array.to_list items)) 
| FOp (op, a, b) ->
    Printf.sprintf
      "FOp (%s, %s, %s)"
      (string_of_op op) (string_of_t a) (string_of_t b)
| IntOp (op, a, b) ->
    Printf.sprintf
      "IntOp (%s, %s, %s)"
      (string_of_op op) (string_of_t a) (string_of_t b)
| ArrayGet (arr, i) ->
    Printf.sprintf "ArrayGet (%s, %s)" (string_of_t arr) (string_of_t i)
| ArraySet (arr, i, newval) ->
    Printf.sprintf
      "ArraySet (%s, %s, %s)"
      (string_of_t arr) (string_of_t i) (string_of_t newval)
| Let ((n, e), e') ->
    Printf.sprintf
      "Let (%s, %s, %s)"
      n (string_of_t e) (string_of_t e')

and string_of_items items =
  List.fold_left ( ^ ) "" (List.map string_of_t items)

and string_of_t {typ; e; lets} =
  List.fold_left ( ^ ) ""
    (List.map
    (fun (n, e) ->
       Printf.sprintf
         "Implicit let: %s = %s\n" n (string_of_t e))
    lets)
  ^
  string_of_t' typ e

