(* Uses the tiny-ocaml simple AST *)

type tiny =
  Int of int
| Bool of bool
| Op of ((int -> int -> int) * tiny * tiny)
| And of (tiny * tiny)
| Or of (tiny * tiny)
| Cmp of ((int -> int -> bool) * tiny * tiny)
| If of (tiny * tiny * tiny)
| Let of (string * tiny * tiny)
| Fun of (bool * (string * tiny) list * tiny)
| App of (tiny * tiny)

let isvalue = function
  Int _ | Bool _ | Fun _ -> true
| _ -> false

(* 1 + 2 * 3 *)
let arith =
  Op (( + ), Int 1, Op (( * ), Int 2, Int 3))

let logicaland =
  And (Bool true, Bool false)

(* Evaluate one step, assuming not already a value *)
let rec eval = function
  Op (op, Int a, Int b) -> Int (op a b)
| Op (op, Int a, b) -> Op (op, a, eval b)
| Op (op, a, b) -> Op (op, eval a, b)
| And (Bool false, _) -> Bool false
| And (Bool true, Bool b) -> Bool b
| And (Bool true, b) -> eval b
| And (a, b) -> And (eval a, b)
| Or (Bool true, _) -> Bool true
| Or (Bool false, Bool b) -> Bool b
| Or (Bool false, b) -> eval b
| Or (a, b) -> And (eval a, b)
| Cmp (op, Int a, Int b) -> Int (op a b)
| Cmp (op, Int a, b) -> Cmp (op, Int a, eval b)
| Cmp (op, a, b) -> Cmp (op, eval a, b)
| If (Bool true, a, _) -> a
| If (Bool false, _, b) -> b
| If (cond, a, b) -> If (eval cond, a, b)
| Let (var, e1, e2) -> assert false
| Fun (recursive, bindings, e) -> assert false
| App (f, x) -> assert false
| _ -> failwith "Eval: already a value"
  
