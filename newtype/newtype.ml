type binding = string * t

and bindings = binding list

and t =
  {t : t';
   lets : (bool * bindings) list} (* The implicit value-lets around any expression *)

and t' =
  Int of int
| Bool of bool
| Var of string
| IfThenElse of t * t * t
| Times of t * t
| Minus of t * t
| Equals of t * t
| Let of bool * binding list * t
| Apply of t * t
| Function of string * t 

let rec is_value_t' = function
  IfThenElse (a, b, c) -> is_value a && is_value b && is_value c
| Times (a, b) | Minus (a, b) | Equals (a, b) | Apply (a, b) -> is_value a && is_value b
| Let (_, bs, a) -> is_value a && List.for_all is_value_binding bs
| Var _ -> false
| Int _ | Bool _ | Function _ -> true

and is_value_binding (_, x) = is_value x

and is_value x = is_value_t' x.t

let eval env e =
  match e.t with
    Int i -> e
  | Bool b -> e
  | Var v -> eval env (lookup env v)
  | IfThenElse ({t = Bool b}, x, y) -> eval env (if b then x else y)
  | IfThenElse (c, x, y) -> eval env (IfThenElse (eval env c, x, y))
  | Times (x, y) -> mkt (Int (eval env x * eval env y))
  | Minus (x, y) -> mkt (Int (eval env x - eval env y))
  | Equals (x, y) -> mkt (Int (eval env x = eval env y))
  | Apply ({t = Function (v, b)}, y) -> eval ((v, eval env y)::env) b
  | Apply (f, y) -> eval env (Apply (eval env f, eval env y))
  | Let (recflag, bindings, e) ->
      (* If bindings not values, evaluate, put as implicit let around 'e' and
       evaluate e with new things in the env. *)

let mkt x = {t = x; lets = []}

(* let rec factorial x = if x = 0 then 1 else x * factorial (x - 1) in factorial 4 *)
let factorial =
  Let (true,
       [("factorial",
         mkt (IfThenElse (mkt (Equals (mkt (Var "x"), mkt (Int 0))),
                     mkt (Int 1),
                     mkt (Times (mkt (Var "x"), mkt (Apply (mkt (Var "factorial"), mkt (Minus (mkt (Var "x"), mkt (Int 1))))))))))],
       (mkt (Apply (mkt (Var "factorial"), mkt (Int 4)))))

let _ = "foo"

