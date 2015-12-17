(* Uses the tiny-ocaml simple AST *)

type tiny =
  Int of int                                     (* 1 *)
| Bool of bool                                   (* false *)
| Var of string                                  (* x *)
| Op of ((int -> int -> int) * tiny * tiny)      (* + - / * *)
| And of (tiny * tiny)                           (* && *)
| Or of (tiny * tiny)                            (* || *)
| Cmp of ((int -> int -> bool) * tiny * tiny)    (* < > <> = <= >= *)
| If of (tiny * tiny * tiny)                     (* if e then e1 else e2 *)
| Let of (bool * (string * tiny) list * tiny)    (* let (rec) x = e [and y = e'] in e'' *)
| Fun of (string * tiny)                         (* fun x -> e *)
| App of (tiny * tiny)                           (* e e' *)

(* Predicate on value-ness of expressions. Var _ is here for convenience *)
let is_value = function
  Int _ | Bool _ | Fun _ | Var _ -> true
| _ -> false

(* 1 + 2 * 3 *)
let arith =
  Op (( + ), Int 1, Op (( * ), Int 2, Int 3))

(* true && false *)
let logicaland =
  And (Bool true, Bool false)

(* (fun x -> x + 1) 2 *)
let funapp =
  App (Fun ("x", Op (( + ), Var "x", Int 1)), Int 2)

let rec substitute n v = function
  Int i -> Int i
| Bool b -> Bool b
| Var x when x = n -> v
| Var x -> Var x
| Op (op, a, b) -> Op (op, substitute n v a, substitute n v b)
| And (a, b) -> And (substitute n v a, substitute n v b)
| Or (a, b) -> Or (substitute n v a, substitute n v b)
| Cmp (cmp, a, b) -> Cmp (cmp, substitute n v a, substitute n v b)
| If (e, e1, e2) -> If (substitute n v e, substitute n v e1, substitute n v e2)
| Let (recursive, bindings, e) ->
    if List.mem n (List.map fst bindings) then
      Let (recursive, bindings, if not recursive then substitute n v e else e)
    else
      Let (recursive, substitute_bindings n v bindings, substitute n v e)
| Fun (x, body) ->
    if x = n then Fun (x, body) else Fun (x, substitute n v body)
| App (f, x) -> App (substitute n v f, substitute n v x)

and substitute_bindings n v l =
  let vs, es = List.split l in
    List.combine vs (List.map (substitute n v) es)

(* Evaluate one step, assuming not already a value *)
let rec eval = function
  Op (op, Int a, Int b) -> Int (op a b)
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
| Cmp (op, Int a, Int b) -> Bool (op a b)
| Cmp (op, Int a, b) -> Cmp (op, Int a, eval b)
| Cmp (op, a, b) -> Cmp (op, eval a, b)
| If (Bool true, a, _) -> a
| If (Bool false, _, b) -> b
| If (cond, a, b) -> If (eval cond, a, b)
| Let (recursive, [(n, v)], e) ->
    if is_value v then substitute n v e else
      Let (recursive, [(n, eval v)], e)
| App (Fun (n, body) as f, x) ->
    if is_value x then substitute n x body else App (f, eval x)
| App (f, x) -> App (eval f, x)
| _ -> failwith "Eval: already a value"
 
(* Evaluate all the way to a value. *)
let rec until_value e =
  if is_value e then e else until_value (eval e)

