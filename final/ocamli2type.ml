open Types

type op = Add | Sub | Mul | Div

type pattern =
  PatAny

type t' =
  Value of Obj.t
| Var of string
| ArrayExpr of t array (* Array not yet a value e.g [|1 + 2; 3|] *)
| Cons of t * t (* Cons part of list literal which is not yet a value e.g [1 + 2; 3] *)
| Append of t * t
| IntOp of op * t * t
| FOp of op * t * t
| ArrayGet of t * t
| ArraySet of t * t * t
| Let of binding * t
| Match of t * case list

and t =
  {typ : Types.type_desc;
   e : t';
   lets : binding list}

and binding = string * t

and case = pattern * t option * t

let string_of_ocaml_type = function
  Tvar (Some x) -> x
| Tvar None -> "_"
| Tarrow (_, _, _, _) -> "Tarrow"
| Ttuple _ -> "Ttuple"
| Tconstr (path, _, _) -> "Tconstr " ^ Path.name path
| Tobject (_, _) -> "Tobject"
| Tfield (_, _, _, _) -> "Tfield"
| Tnil -> "Tnil"
| Tlink _ -> "Tlink"
| Tsubst _ -> "Tsubst"
| Tvariant _ -> "Tvariant"
| Tunivar _ -> "Tunivar"
| Tpoly (_, _) -> "Tpoly"
| Tpackage (_, _, _) -> "Tpackage"

let is_value_t' = function
  Value _ -> true
| ArrayExpr _ | Append _ | Cons _ | IntOp _ | FOp _
| ArrayGet _ | ArraySet _ | Let _ | Var _ | Match _ -> false

let is_value {e} = is_value_t' e

let rec should_be_value_t' = function
  x when is_value_t' x -> true
| ArrayExpr arr -> Array.for_all should_be_value arr
| Cons (h, t) -> should_be_value h && should_be_value t
| _ -> false

and should_be_value {e} = should_be_value_t' e

(* List the names in an expression, including any implicit let-bindings. *)
let rec names_in_t' = function
  Value _ -> []
| Var x -> [x]
| ArrayExpr elts -> List.flatten (Array.to_list (Array.map names_in elts))
| IntOp (_, e, e') | FOp (_, e, e') | ArrayGet (e, e') | Cons (e, e') | Append (e, e') -> names_in e @ names_in e'
| Let (binding, e) -> names_in_binding binding @ names_in e
| ArraySet (e, e', e'') -> names_in e @ names_in e' @ names_in e''
| Match (e, cases) -> names_in e @ List.flatten (List.map names_in_case cases)

(* FIXME What to do here? Are we supposed to count the pattern name and all uses and what about shadows? The only use of this function at the moment is in the writer for not showing occluded lets. So combine it with that or specify it properly... *)
and names_in_case (pat, guard, e) =
  names_in e @
  begin match guard with None -> [] | Some e -> names_in e end

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
  | Tconstr (p, [elt_t], _) when Path.name p = "list" ->
      if Obj.is_int value then Tinyocaml.Nil else
        Tinyocaml.Cons
          (tinyocaml_of_ocaml_heap_value (find_type_desc elt_t) (Obj.field value 0),
           tinyocaml_of_ocaml_heap_value typ (Obj.field value 1))
  | _ -> failwith "tinyocaml_of_ocaml_heap_value: unknown type"

let rec string_of_t' typ = function
  Value x -> Pptinyocaml.to_string (tinyocaml_of_ocaml_heap_value typ x)
| Var x -> Printf.sprintf "Var %s" x
| ArrayExpr items ->
    Printf.sprintf "[|%s|]" (string_of_items (Array.to_list items)) 
| Cons (a, b) ->
    Printf.sprintf
      "Cons (%s, %s)" (string_of_t a) (string_of_t b)
| Append (a, b) ->
    Printf.sprintf
      "Append (%s, %s)" (string_of_t a) (string_of_t b)
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
| Match (e, cases) ->
    Printf.sprintf "Match (%s, %s)" (string_of_t e) "<cases>"

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

