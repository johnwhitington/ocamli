(* Uses the tiny-ocaml simple AST *)
type op = Add | Sub | Mul | Div

type comparison = LT | EQ | GT | EQLT | EQGT | NEQ

type tiny =
  Int of int                                     (* 1 *)
| Bool of bool                                   (* false *)
| Var of string                                  (* x *)
| Op of (op * tiny * tiny)                       (* + - / * *)
| And of (tiny * tiny)                           (* && *)
| Or of (tiny * tiny)                            (* || *)
| Cmp of (comparison * tiny * tiny)              (* < > <> = <= >= *)
| If of (tiny * tiny * tiny)                     (* if e then e1 else e2 *)
| Let of (string * tiny * tiny)                  (* let x = e in e' *)
| LetRec of (string * tiny * tiny)               (* let rec x = e in e' *)
| Fun of (string * tiny)                         (* fun x -> e *)
| App of (tiny * tiny)                           (* e e' *)

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
| Let (n, v, e) -> if is_value v then substitute n v e else Let (n, v, e)
| LetRec (n, v, e) ->
    if is_value v then substitute n v e else LetRec (n, v, e) (* FIXME *)
| App (Fun (n, body) as f, x) ->
    if is_value x then substitute n x body else App (f, eval x)
| App (f, x) -> App (eval f, x)
| Var _ -> failwith "Expression not closed: unknown variable"
| Int _ | Bool _ | Fun _ -> failwith "Eval: already a value"
 
(* Evaluate all the way to a value. *)
let rec until_value e =
  if is_value e then e else until_value (eval e)

(* Example programs *)

(* 1 + 2 * 3 *)
let arith =
  Op (Add, Int 1, Op (Mul, Int 2, Int 3))

(* true && false *)
let logicaland =
  And (Bool true, Bool false)

(* (fun x -> x + 1) 2 *)
let funapp =
  App (Fun ("x", Op (Add, Var "x", Int 1)), Int 2)

(* Function *)
let func =
  Let ("f", Fun ("x", Op (Add, Var "x", Int 1)), App (Var "f", Int 6))

(* let x = 1 in let x = 2 in let y = 3 in x + y *)
let lets =
  Let ("x", Int 1,
    Let ("x", Int 2,
      Let ("y", Int 3, Op (Add, Var "x", Var "y"))))

(* let rec factorial x =
     if x = 1 then 1 else x * factorial (x - 1)
   in
     factorial 4 *)
let factorial =
  LetRec
    ("factorial",
     Fun ("x",
       If (Cmp (EQ, Var "x", Int 1),
           Var "x",
           App (Var "factorial", Op (Sub, Var "x", Int 1)))),
     App (Var "factorial", Int 4))
