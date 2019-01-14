open Types

(* Set this to false to debug failures in tinyocaml_of_ocaml_heap_value *)
let showvals = ref true

type op = Add | Sub | Mul | Div

type patconstant =
  IntConstant of int

type pattern =
  PatAny
| PatVar of string
| PatConstr of string * pattern list
| PatConstant of patconstant

type t' =
  Value of Obj.t
| Function of case list * env
| Apply of t * t list
| Var of string
| ArrayExpr of t array (* Array not yet a value e.g [|1 + 2; 3|] *)
| Cons of t * t (* Cons part of list literal which is not yet a value e.g [1 + 2; 3] *)
| Append of t * t
| IntOp of op * t * t
| FOp of op * t * t
| ArrayGet of t * t
| ArraySet of t * t * t
| Let of bool * binding * t
| Match of t * case list
| Struct of t list
| LetDef of bool * binding

and t =
  {typ : Types.type_desc;
   e : t';
   lets : env}

and env = envitem list

and envitem = bool * binding list ref

and binding = string * t

and case = pattern * t option * t

let rec find_type_desc {desc} =
  match desc with
    Tlink x -> find_type_desc x
  | typ -> typ

let rec string_of_ocaml_type = function
  Tvar (Some x) -> x
| Tvar None -> "Tvar None"
| Tnil -> "Tnil"
| Tarrow (arg_label, a, b, commutable) ->
    Printf.sprintf
      "Tarrow (%s, %s)"
      (string_of_ocaml_type a.desc)
      (string_of_ocaml_type b.desc)
| Tconstr (path, types, abbrev_memo) ->
    "Tconstr " ^ Path.name path
  ^ "("
  ^ List.fold_left ( ^ ) "" (List.map (fun x -> string_of_ocaml_type x.desc ^ " ") types)
  ^ ")"
| Ttuple _ -> "Ttuple"
| Tobject (_, _) -> "Tobject"
| Tfield (_, _, _, _) -> "Tfield"
| Tlink x -> string_of_ocaml_type (find_type_desc x)
| Tsubst _ -> "Tsubst"
| Tvariant _ -> "Tvariant"
| Tunivar _ -> "Tunivar"
| Tpoly (_, _) -> "Tpoly"
| Tpackage (_, _, _) -> "Tpackage"

let rec is_value_t' = function
  Value _ | Function _ -> true
| ArrayExpr _ | Append _ | Cons _ | IntOp _ | FOp _
| ArrayGet _ | ArraySet _ | Let _ | Var _ | Match _ | Apply _ -> false
| Struct l -> List.for_all is_value l
| LetDef (_, (_, e)) -> is_value e

and is_value {e} = is_value_t' e

let rec should_be_value_t' = function
  x when is_value_t' x -> true
| ArrayExpr arr -> Array.for_all should_be_value arr
| Cons (h, t) -> should_be_value h && should_be_value t
| _ -> false

and should_be_value {e} = should_be_value_t' e

(* List the names in an expression, including any implicit let-bindings. *)
let rec names_in_t' = function
  Value _ -> []
| Function (cases, env) -> List.flatten (List.map names_in_case cases)
| Apply (a, es) -> names_in a @ List.flatten (List.map names_in es)
| Var x -> [x]
| ArrayExpr elts -> List.flatten (Array.to_list (Array.map names_in elts))
| IntOp (_, e, e') | FOp (_, e, e') | ArrayGet (e, e') | Cons (e, e') | Append (e, e') ->
    names_in e @ names_in e'
| Let (_, binding, e) -> names_in_binding binding @ names_in e
| ArraySet (e, e', e'') -> names_in e @ names_in e' @ names_in e''
| Match (e, cases) -> names_in e @ List.flatten (List.map names_in_case cases)
| Struct l -> List.flatten (List.map names_in l)
| LetDef (_, (n, e)) -> n :: names_in e

(* FIXME What to do here? Are we supposed to count the pattern name and all
 * uses and what about shadows? The only use of this function at the moment is
 * in the writer for not showing occluded lets. So combine it with that or
 * specify it properly... *)
and names_in_case (pat, guard, e) =
  names_in e @
  begin match guard with None -> [] | Some e -> names_in e end

and names_in_binding (n, e) =
  n :: names_in e 

and names_in_envitem (_, r) =
  List.flatten (List.map names_in_binding !r)

and names_in {lets; e} =
  List.flatten (List.map names_in_envitem lets) @ names_in_t' e

let string_of_op = function
    Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div" 


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
  | _ -> if !showvals
           then failwith "tinyocaml_of_ocaml_heap_value: unknown type"
           else Tinyocaml.String (Bytes.of_string "<unknown val>")

let rec string_of_t' typ = function
  Value x -> Ocamli2print.to_string (tinyocaml_of_ocaml_heap_value typ x)
| Function (cases, env) ->
    Printf.sprintf "Function (%s, env = %s)" (string_of_cases cases) (string_of_env env)
| Apply (e, args) ->
    Printf.sprintf "Apply (%s, [%s])" (string_of_t e) (string_of_items args)
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
| Let (recflag, (n, e), e') ->
    Printf.sprintf
      "Let (%b, %s, %s, %s)"
      recflag n (string_of_t e) (string_of_t e')
| Match (e, cases) ->
    Printf.sprintf "Match (%s, %s)" (string_of_t e) "<cases>"
| Struct l ->
    Printf.sprintf "Struct:%s\n"
      (List.fold_left (fun x y -> x ^ "\n" ^ y) "" (List.map string_of_t l))
| LetDef (recflag, (n, e)) ->
    Printf.sprintf "LetDef %b (%s, %s)"
      recflag n (string_of_t e)

and string_of_case (p, _, e) = (* FIXME guard *)
  Printf.sprintf "[%s -> %s]" (string_of_pattern p) (string_of_t e)
     
and string_of_cases cases =
  List.fold_left ( ^ ) "" (List.map (fun x -> string_of_case x ^ " ") cases)

and string_of_pattern = function
  PatAny -> "_"
| PatVar v -> v
| PatConstr (constr, pats) ->
    "PatConstr " ^ constr ^ "("
  ^ List.fold_left ( ^ ) "" (List.map (fun x -> string_of_pattern x ^ ", ") pats)
  ^ ")"
| PatConstant (IntConstant i) -> string_of_int i

and string_of_items items =
  List.fold_left ( ^ ) "" (List.map (fun x -> string_of_t x ^ ";") items)

and string_of_t {typ; e; lets} =
  List.fold_left ( ^ ) ""
    (List.map
    (fun (recflag, r) ->
       Printf.sprintf "{%b, %s}" recflag (string_of_bindings !r))
    lets)
  ^
  "{typ = " ^ string_of_ocaml_type typ ^ "}" 
  ^
  string_of_t' typ e

and string_of_bindings bs =
  List.fold_left ( ^ ) "" (List.map string_of_binding bs)

and string_of_binding (n, e) =
  Printf.sprintf "%s = %s; " n (string_of_t e)

and string_of_envitem (recflag, {contents}) =
  Printf.sprintf "(%b, %s)" recflag (string_of_bindings contents)

and string_of_env es =
  List.fold_left ( ^ ) "" (List.map (fun e -> string_of_envitem e ^ ";\n") es)


