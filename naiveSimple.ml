(* Uses the tiny-ocaml simple AST *)
open Tinyocaml
open Evalutils

let bold, ul, code_end = ("\x1b[1m", "\x1b[4m", "\x1b[0m")

type t = Tinyocaml.t

let calc = TinyocamlUtils.calc

let comp = TinyocamlUtils.comp

(* Predicate on value-ness of expressions. Var _ is here for convenience *)
let is_value = TinyocamlUtils.is_value

(* Substitute a value for a name in an expresion *)
let substitute = TinyocamlUtils.substitute

(* If not a value, underline it as the next redex *)
let underline x =
  Control (Underline, x)

let rec underline_redex e =
  match e with
    Control (c, x) -> Control (c, underline_redex x)
  | Op (_, Int _, Int _) -> underline e
  | Op (op, Int a, b) -> Op (op, Int a, underline b)
  | Op (op, a, b) -> Op (op, underline a, b)
  | And (Bool false, _) -> underline e
  | And (Bool true, Bool _) -> underline e
  | And (Bool true, b) -> And (Bool true, underline b)
  | And (a, b) -> And (underline a, b)
  | Or (Bool true, _) -> underline e
  | Or (Bool false, Bool b) -> underline e
  | Or (Bool false, b) -> Or (Bool false, underline b)
  | Cmp (_, Int _, Int _) -> underline e
  | Cmp (op, Int a, b) -> Cmp (op, Int a, underline b)
  | Cmp (op, a, b) -> Cmp (op, underline a, b)
  | If (Bool _, _, _) -> underline e
  | If (cond, a, b) -> If (underline cond, a, b)
  | Let (n, v, e') ->
      if is_value v then underline e else Let (n, underline v, e')
  | LetRec (n, Fun (var, body), e) -> LetRec (n, Fun (var, body), underline e)
  | App (Fun f, x) ->
      if is_value x then underline e else App (Fun f, underline x)
  | App (f, x) -> App (underline f, x)
  | _ -> e (* A value, so no redex *)

(* Evaluate one step, assuming not already a value *)
let rec eval = function
| Control (_, x) -> eval x
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

let to_string x = Pptinyocaml.string_of_tiny (underline_redex x) 

let tiny x = x

let last () = Unknown

let peek _ = Unknown

