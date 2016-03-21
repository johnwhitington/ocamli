open Tinyocaml

(* if true then 1 else 2 *)
let if1 = If (Bool true, Int 1, Int 2)
(* if true then if true then 1 else 2 else 3 *)
let if2 = If (Bool true, If (Bool true, Int 1, Int 2), Int 3)
(* if true then 1 else if true then 2 else 3 *)
let if3 = If (Bool true, Int 1, If (Bool true, Int 2, Int 3))
(* if false then 1 else 2 + 2 *)
let if4 = If (Bool false, Int 1, Op (Add, Int 2, Int 2))
(* (if false then 1 else 2) + 2 *)
let if5 = Op (Add, If (Bool false, Int 1, Int 2), Int 2)

(* a && b && c *)
let bool1 = And (Bool true, And (Bool true, Bool true))
(* (a && b) && c *)
let bool2 = And (And (Bool true, Bool true), Bool true)
(* a || b || c *)
let bool3 = Or (Bool true, Or (Bool true, Bool true))
(* (a || b) || c *)
let bool4 = Or (Or (Bool true, Bool true), Bool true)
(* a && b || c *)
let bool5 = Or (And (Bool true, Bool true), Bool true)

(* 1 + 2 * 3 *)
let arith =
  Op (Add, Int 1, Op (Mul, Int 2, Int 3))

(* (1 + 2) * 3 *)
let arithplus =
  Op (Mul, Op (Add, Int 1, Int 2), Int 3)

(* 1 + (2 + 3) *)
let plusr =
  Op (Add, Int 1, Op (Add, Int 2, Int 3))

let plusl =
  Op (Add, Op (Add, Int 1, Int 2), Int 3)

(* true && false *)
let logicaland =
  And (Bool true, Bool false)

let mkfun fname fexp =
  Fun {fname; fexp; fper = false}

(* (fun x -> x + 1) 2 *)
let funapp =
  App (mkfun "x" (Op (Add, Var "x", Int 1)), Int 2)

(* Function *)
let func =
  Let ("f", mkfun "x" (Op (Add, Var "x", Int 1)), App (Var "f", Int 6))

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
     mkfun "x"
       (If (Cmp (EQ, Var "x", Int 1),
           Var "x",
           App (Var "factorial", Op (Sub, Var "x", Int 1)))),
     App (Var "factorial", Int 4))
