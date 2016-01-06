(* Uses the tiny-ocaml simple AST. Not step by step, but straight to value. *)
open Tinyocaml
open Evalutils

type t = Tinyocaml.t

let calc = TinyocamlUtils.calc

let comp = TinyocamlUtils.comp

(* Predicate on value-ness of expressions. Var _ is here for convenience *)
let is_value = TinyocamlUtils.is_value

(* Substitute a value for a name in an expresion *)
let substitute = TinyocamlUtils.substitute

(* Evaluate one step, assuming not already a value *)
let rec eval = function
  Op (op, Int a, Int b) -> Int (calc op a b)
| Op (op, a, b) -> eval (Op (op, eval a, eval b))
| And (Bool false, _) -> Bool false
| And (Bool true, Bool b) -> Bool b
| And (Bool true, b) -> eval b
| And (a, b) -> eval (And (eval a, b))
| Or (Bool true, _) -> Bool true
| Or (Bool false, Bool b) -> Bool b
| Or (Bool false, b) -> eval b
| Or (a, b) -> eval (And (eval a, b))
| Cmp (op, Int a, Int b) -> Bool (comp op a b)
| Cmp (op, a, b) -> eval (Cmp (op, eval a, eval b))
| If (Bool true, a, _) -> eval a
| If (Bool false, _, b) -> eval b
| If (cond, a, b) -> eval (If (eval cond, a, b))
| Let (n, v, e) ->
    if is_value v
      then eval (substitute n v e)
      else eval (Let (n, eval v, e))
| LetRec (n, Fun (var, body), e) ->
    let v = Fun (var, LetRec (n, Fun (var, body), body)) in
      eval (substitute n v e)
| LetRec (n, v, e) ->
    if is_value v then eval (substitute n v e) else eval (LetRec (n, eval v, e))
| App (Fun (n, body) as f, x) ->
    if is_value x then eval (substitute n x body) else eval (App (f, eval x))
| App (f, x) -> eval (App (eval f, x))
| Var _ -> failwith "Expression not closed: unknown variable"
| (Int _ | Bool _ | Fun _) as x -> x
 
(* Evaluate all the way to a value. *)
let rec until_value e =
  if is_value e then e else until_value (eval e)

let init x = Tinyocaml.of_real_ocaml (getexpr x)

let next e =
  try
    if is_value e then IsValue else Next (eval e) 
  with
    _ -> Malformed "naiveSimple"

let tree x = makestructure (Tinyocaml.to_real_ocaml x)

