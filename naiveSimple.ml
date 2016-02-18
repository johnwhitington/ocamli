(* Uses the tiny-ocaml simple AST *)
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
| Control (_, x, _) -> eval x
| Op (op, Int a, Int b) -> Int (calc op a b)
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
    let v = Fun (var, LetRec (n, Fun (var, body), body)) in
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

let init x = Tinyocaml.of_real_ocaml (getexpr x)

let next e =
  try
    if is_value e then IsValue else Next (eval e) 
  with
    _ -> Malformed "naiveSimple"

let tree x = makestructure (Tinyocaml.to_real_ocaml x)

