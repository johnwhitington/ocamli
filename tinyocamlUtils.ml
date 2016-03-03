open Tinyocaml

let calc = function
  Add -> (+) | Sub -> (-) | Mul -> ( * ) | Div -> (/)

let comp = function
  LT -> (<) | EQ -> (=) | GT -> (>) | EQLT -> (<=) | EQGT -> (>=) | NEQ -> (<>)

(* Substitute a value for a name in an expresion *)
let rec substitute n v = function
  Var x when x = n -> v
| Op (op, a, b) -> Op (op, substitute n v a, substitute n v b)
| And (a, b) -> And (substitute n v a, substitute n v b)
| Or (a, b) -> Or (substitute n v a, substitute n v b)
| Cmp (cmp, a, b) -> Cmp (cmp, substitute n v a, substitute n v b)
| If (e, e1, e2) -> If (substitute n v e, substitute n v e1, substitute n v e2)
| Let (var, e, e') when var = n -> Let (var, substitute n v e, e')
| Let (var, e, e') -> Let (var, substitute n v e, substitute n v e')
| LetRec (var, e, e') when var = n -> LetRec (var, e, e')
| LetRec (var, e, e') -> LetRec (var, substitute n v e, substitute n v e')
| Fun (x, e) -> if x = n then Fun (x, e) else Fun (x, substitute n v e)
| App (f, x) -> App (substitute n v f, substitute n v x)
| x -> x

(* Predicate on value-ness of expressions. *)
let rec is_value = function
  Unit | Int _ | Bool _ | Fun _ -> true
| Record items when
    List.fold_left
      ( && ) true
      (List.map is_value (List.map (fun (_, {contents = e}) -> e) items)) -> true
| _ -> false

let bold, ul, code_end = ("\x1b[1m", "\x1b[4m", "\x1b[0m")

(* If not a value, underline it as the next redex *)
let underline x =
  Control (Underline, x)

exception UnderlineValueUnderLets
exception UnderlineValueUnderLets2

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
    | Let (n, v, e') ->
        if is_value v
          then Let (n, v, underline_redex e')
          else Let (n, underline_redex v, e')
    | LetRec (n, Fun (var, body), e') ->
        LetRec (n, Fun (var, body), underline_redex e')
    | App (Fun f, x) ->
        if is_value x then underline e else App (Fun f, underline_redex x)
    | App (Var v, x) ->
        if is_value x then underline e else App (Var v, underline_redex x)
    | App (f, x) -> App (underline_redex f, x)
    | Seq (a, b) ->
        if is_value a then underline e else Seq (underline_redex a, b)
    | Var _ -> underline e
    | Record items ->
        underline_first_non_value items;
        Record items
    | Field (a, n) ->
        if is_value a then underline e else Field (underline a, n)
    | SetField (a, n, b) ->
        if is_value a then
          if is_value b then underline e else SetField (a, n, underline b)
        else
          SetField (underline a, n, b)
    | Raise _ -> underline e
    | TryWith (a, (s, b)) ->
        if is_value e then underline e else TryWith (underline a, (s, b))
    | _ -> raise UnderlineValueUnderLets
  with
    UnderlineValueUnderLets -> raise UnderlineValueUnderLets2
  | UnderlineValueUnderLets2 -> underline e

and underline_first_non_value items =
  try
    List.iter (fun (_, v) -> if not (is_value !v) then v := underline !v) items
  with
    Exit -> ()

let underline_redex e =
  if is_value e then e else underline_redex e

