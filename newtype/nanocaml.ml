type op = Add | Sub | Mul | Div

let string_of_op = function
  Add -> "Add"
| Sub -> "Sub"
| Mul -> "Mul"
| Div -> "Div"

type binding = string * t

and envitem = bool * binding list ref

and environment = envitem list

and t =
  {t : t';
   lets : (bool * binding list) list}

and t' =
  Int of int
| Bool of bool
| Var of string
| If of t * t * t
| Op of op * t * t
| Equals of t * t
| Let of bool * binding list * t
| LetDef of bool * binding list
| Apply of t * t
| Function of string * environment * t
| Struct of t list

let mkt x =
  {t = x; lets = []}

let rec is_value_t' = function
| Let (_, bs, a) -> is_value a && List.for_all is_value_binding bs
| LetDef (_, bs) -> List.for_all is_value_binding bs
| Struct xs -> List.for_all is_value xs
| Int _ | Bool _ | Function _ -> true
| _ -> false

and is_value_binding (_, x) = is_value x

and is_value x = is_value_t' x.t

let rec string_of_t' = function
  Int i -> Printf.sprintf "Int %i" i
| Bool b -> Printf.sprintf "Bool %b" b
| Var v -> Printf.sprintf "Var %s" v
| If (a, b, c) ->
    Printf.sprintf "If (%s, %s, %s)" (string_of_t a) (string_of_t b) (string_of_t c)
| Op (op, a, b) -> Printf.sprintf "%s (%s, %s)" (string_of_op op) (string_of_t a) (string_of_t b)
| Equals (a, b) -> Printf.sprintf "Equals (%s, %s)" (string_of_t a) (string_of_t b)
| Let (recflag, bindings, e) ->
    Printf.sprintf "Let %b, [%s], %s" recflag (string_of_bindings bindings) (string_of_t e) 
| LetDef (recflag, bindings) ->
    Printf.sprintf "LetDef %b, [%s]" recflag (string_of_bindings bindings)
| Apply (f, x) ->
    Printf.sprintf "Apply (%s, %s)" (string_of_t f) (string_of_t x)
| Function (v, _, e) ->
    Printf.sprintf "Function (%s, %s)" v (string_of_t e)
| Struct ts ->
    Printf.sprintf "Struct %s"
      (List.fold_left (fun x y -> x ^ "\n" ^ y) "" (List.map string_of_t ts))

and string_of_implicit_lets = function
  [] -> ""
| (recflag, bindings)::more ->
     Printf.sprintf "%b, %s\n" recflag (string_of_bindings bindings) ^
     string_of_implicit_lets more

and string_of_t e =
  string_of_implicit_lets e.lets ^
  "\n" ^
  string_of_t' e.t

and string_of_binding (varname, t) =
  Printf.sprintf "Binding %s" varname

and string_of_bindings bs =
  List.fold_left
    (fun a b -> a ^ "\n" ^ b)
    ""
    (List.map string_of_binding bs)

let print_env_item (n, bs) =
  Printf.printf "%b, %s\n" n (string_of_bindings !bs)

let print_env (env : environment) =
  List.iter print_env_item env

let rec lookup_in_bindings v bs =
  match bs with
    [] -> None
  | (v', e)::_ when v = v' -> Some e
  | _::bs -> lookup_in_bindings v bs

let rec lookup_in_environment v (env : environment) =
  match env with
    [] -> failwith "lookup_in_environment"
  | (_, {contents = bindings})::more ->
    match lookup_in_bindings v bindings with
      None -> lookup_in_environment v more
    | Some x -> x

let envitem_of_bindings recflag bindings =
  (recflag, ref bindings)

let rec add_implicit_lets_to_environment env = function
  [] -> env
| (recflag, bindings)::more ->
    add_implicit_lets_to_environment
      ((envitem_of_bindings recflag bindings)::env)
      more

