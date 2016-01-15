open Tinyocaml

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
