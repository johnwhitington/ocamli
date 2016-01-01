(* Uses the tiny-ocaml simple AST *)
open Tinyocaml

let calc = function
  Add -> (+) | Sub -> (-) | Mul -> ( * ) | Div -> (/)

let comp = function
  LT -> (<) | EQ -> (=) | GT -> (>) | EQLT -> (<=) | EQGT -> (>=) | NEQ -> (<>)

(* Predicate on value-ness of expressions. Var _ is here for convenience *)
let is_value = function
  Int _ | Bool _ | Fun _ -> true | _ -> false

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

(* Evaluate one step, assuming not already a value *)
let rec eval = function
  Op (op, Int a, Int b) -> Int (calc op a b)
| Op (op, Int a, b) -> Op (op, Int a, eval b)
| Op (op, a, b) -> Op (op, eval a, b)
| And (Bool false, _) -> Bool false
| And (Bool true, Bool b) -> Bool b
| And (Bool true, b) -> eval b
| And (a, b) -> And (eval a, b)
| Or (Bool true, _) -> Bool true
| Or (Bool false, Bool b) -> Bool b
| Or (Bool false, b) -> eval b
| Or (a, b) -> And (eval a, b)
| Cmp (op, Int a, Int b) -> Bool (comp op a b)
| Cmp (op, Int a, b) -> Cmp (op, Int a, eval b)
| Cmp (op, a, b) -> Cmp (op, eval a, b)
| If (Bool true, a, _) -> a
| If (Bool false, _, b) -> b
| If (cond, a, b) -> If (eval cond, a, b)
| Let (n, v, e) -> if is_value v then substitute n v e else Let (n, eval v, e)
| LetRec (n, Fun (var, body), e) ->
    let v =
      Fun (var, LetRec (n, Fun (var, body), body))
    in
      substitute n v e
| LetRec (n, v, e) ->
    if is_value v then substitute n v e else LetRec (n, eval v, e)
| App (Fun (n, body) as f, x) ->
    if is_value x then substitute n x body else App (f, eval x)
| App (f, x) -> App (eval f, x)
| Var _ -> failwith "Expression not closed: unknown variable"
| Int _ | Bool _ | Fun _ -> failwith "Eval: already a value"
 
(* Evaluate all the way to a value. *)
let rec until_value e =
  if is_value e then e else until_value (eval e)

