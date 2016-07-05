open Tinyocaml

let calc = function
  Add -> (+) | Sub -> (-) | Mul -> ( * ) | Div -> (/)

let comp = function
  LT -> (<) | EQ -> (=) | GT -> (>) | EQLT -> (<=) | EQGT -> (>=) | NEQ -> (<>)

(* Predicate on value-ness of expressions. *)
let rec is_value = function
  Unit | Int _ | Bool _ | Fun _ | Function _
| Int32 _ | Int64 _ | NativeInt _ 
| OutChannel _ | InChannel _ | Char _ | String _ | Nil -> true
| Record items when
    List.for_all is_value (List.map (fun (_, {contents = e}) -> e) items) -> true
| Struct (_, items) when
    List.for_all is_value items -> true
| Tuple items when
    List.for_all is_value items -> true
| Array items when
    Array.for_all is_value items -> true
| Constr (_, None) -> true
| Constr (_, Some t) -> is_value t
| Cons (e, e') when
    is_value e && is_value e' -> true
| LetDef (_, bindings) when
    List.for_all (fun (_, e) -> is_value e) bindings -> true
| Let (_, bindings, e) when
    List.for_all (fun (_, e) -> is_value e) bindings && is_value e -> true
| ExceptionDef _ | TypeDef _ | ModuleBinding _ -> true
| _ -> false

let bold, ul, code_end = ("\x1b[1m", "\x1b[4m", "\x1b[0m")

(* If not a value, underline it as the next redex *)
let underline x =
  Control (Underline, x)

exception UnderlineValueUnderLets
exception UnderlineValueUnderLets2

let fastcurry = ref false

let rec underline_redex e =
  try
    match e with
      Control (c, x) -> Control (c, underline_redex x)
    | Op (_, (Int _ | Var _), (Int _ | Var _)) -> underline e
    | Op (op, ((Int _ | Var _) as a), b) -> Op (op, a, underline_redex b)
    | Op (op, a, b) -> Op (op, underline_redex a, b)
    | And (Bool false, _) -> underline e
    | And (Bool true, Bool _) -> underline e
    | And (Bool true, b) -> And (Bool true, underline_redex b)
    | And (a, b) -> And (underline_redex a, b)
    | Or (Bool true, _) -> underline e
    | Or (Bool false, Bool b) -> underline e
    | Or (Bool false, b) -> Or (Bool false, underline_redex b)
    | Cmp (_, (Int _ | Var _), (Int _ | Var _)) -> underline e
    | Cmp (op, ((Int _ | Var _) as a), b) -> Cmp (op, a, underline_redex b)
    | Cmp (op, a, b) -> Cmp (op, underline_redex a, b)
    | If (Bool _, _, _) -> underline e
    | If (cond, a, b) -> If (underline_redex cond, a, b)
    | Let (recflag, bindings, e') ->
        if List.for_all (fun (_, v) -> is_value v) bindings then
          Let (recflag, bindings, underline_redex e')
        else
          Let (recflag, underline_first_non_value_binding bindings, e')
    | LetDef (recflag, bindings) ->
        if List.for_all (fun (_, v) -> is_value v) bindings then
          failwith "letdef already a value"
        else
          LetDef (recflag, underline_first_non_value_binding bindings)
    | App (Fun f, x) ->
        if is_value x then underline e else App (Fun f, underline_redex x)
    | App (Function f, x) ->
        if is_value x then underline e else App (Function f, underline_redex x)
    | App (Var v, x) ->
        if is_value x then underline e else App (Var v, underline_redex x)
    | App (App _, _) when !fastcurry ->
        underline_curry e
    | App (f, x) -> App (underline_redex f, x)
    | Seq (a, b) ->
        if is_value a then underline e else Seq (underline_redex a, b)
    | While (we, we', copy_we, copy_we') ->
        if not (is_value we) then While (underline_redex we, we', copy_we, copy_we') else
        if not (is_value we') then While (we, underline_redex we', copy_we, copy_we') else
          underline e
    | For (n, fe, updown, fe', fe'', copy_fe'') ->
        if not (is_value fe) then
          For (n, underline_redex fe, updown, fe', fe'', copy_fe'')
        else if not (is_value fe') then
          For (n, fe, updown, underline_redex fe', fe'', copy_fe'')
        else if not (is_value fe'') then
          For (n, fe, updown, fe', underline_redex fe'', copy_fe'')
        else
          underline e
    | Var _ -> underline e
    | Record items ->
        underline_first_non_value_ref items;
        Record items
    | Field (a, n) ->
        if is_value a then underline e else Field (underline a, n)
    | SetField (a, n, b) ->
        if is_value a then
          if is_value b then underline e else SetField (a, n, underline b)
        else
          SetField (underline a, n, b)
    | Raise _ -> underline e
    | TryWith (a, cases) ->
        if is_value a then underline e else TryWith (underline a, cases)
    | CallBuiltIn (name, args, fn) ->
        if List.for_all is_value args
          then underline e
          else CallBuiltIn (name, underline_first_non_value args, fn)
    | Struct (b, ls) ->
        if List.for_all is_value ls
          then failwith "module already a value"
          else Struct (b, underline_first_non_value ls)
    | Tuple ls ->
        if List.for_all is_value ls
          then failwith "tuple already a value"
          else Tuple (underline_first_non_value ls)
    | Array items ->
        if Array.for_all is_value items
          then failwith "tuple already a value"
          else Array (underline_first_non_value_array items)
    | Constr (n, Some t) ->
        if is_value t
          then failwith "constr already a value"
          else Constr (n, Some (underline_redex t))
    | Cons (x, y) ->
        if is_value x then Cons (x, underline_redex y) else Cons (underline x, y)
    | Match (x, patmatch) ->
        if is_value x then underline e else Match (underline_redex x, patmatch)
    | _ ->
        raise UnderlineValueUnderLets
  with
    UnderlineValueUnderLets -> raise UnderlineValueUnderLets2
  | UnderlineValueUnderLets2 -> underline e

(* 1) Underline the first function which is not a value, if there is one, or else *)
(* 2) Underline the last argument which is not a value, if there is one, or else *)
(* 3) We are ready to apply, return None. *)
and underline_curry_inner e =
  match e with
  | App (f, x) when not (is_value x) -> Some (App (f, underline_redex x))
  | App (App _ as f', x') ->
      begin match underline_curry_inner f' with
        None -> None
      | Some f'' -> Some (App (f'', x'))
      end
  | App (f, x) when not (is_value f) -> Some (App (underline_redex f, x))
  | _ -> None

and underline_curry e =
  match underline_curry_inner e with
    None -> underline e
  | Some x -> x

and underline_first_non_value = function
  [] -> []
| h::t ->
    if is_value h
      then h::underline_first_non_value t
      else underline_redex h::t

and underline_first_non_value_array x =
  Array.of_list (underline_first_non_value (Array.to_list x))

and underline_first_non_value_binding = function
  [] -> []
| (k, v)::t ->
    if is_value v
      then (k, v)::underline_first_non_value_binding t
      else (k, underline_redex v)::t

and underline_first_non_value_ref items =
  try
    List.iter (fun (_, v) -> if not (is_value !v) then v := underline_redex !v) items
  with
    Exit -> ()

let underline_redex e =
  if is_value e then e else underline_redex e

let rec strip_control = function
  Control (_, e) -> e
| x -> Tinyocaml.recurse strip_control x

let rec remove_named_recursive_functions all fns = function
  Let (true, [(PatVar n, v)], e) ->
    let r = Tinyocaml.recurse (remove_named_recursive_functions all fns) e in
      if all || List.mem n fns then r else Let (true, [(PatVar n, v)], r)
| x -> Tinyocaml.recurse (remove_named_recursive_functions all fns) x

