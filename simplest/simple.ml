(* Simple evaluator with no fenvs, but env *)
type t =
  Int of int
| Var of string
| Add of t * t
| Fun of string * t
| App of t * t

let rec eval env = function
| Int x -> Int x
| Fun (n, b) -> Fun (n, b)
| Var x -> eval env (List.assoc x env)
| Add (a, b) ->
    begin match eval env a, eval env b with
      Int x, Int y -> Int (x + y)
    | _ -> failwith "bad add"
    end
| App (a, b) ->
    match eval env a with
      Fun (name, body) ->
        (* Use a Let to record it? *)
    | _ -> failwih "app"

(* (fun x -> x + 1) 4 *)
let f = App (Fun ("x", Add (Var "x", Int 1)), Int 4)

(* (fun x -> fun y -> x + y) 4 5 *)
let f2 = App (App (Fun ("x", Fun ("y",  Add (Var "x", Var "y"))), Int 4), Int 5)
