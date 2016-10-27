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
| LetDef of bool * binding list
| Apply of t * t
| Function of string * t
| Struct of t list

let rec is_value_t' = function
  IfThenElse (a, b, c) -> is_value a && is_value b && is_value c
| Times (a, b) | Minus (a, b) | Equals (a, b) | Apply (a, b) -> is_value a && is_value b
| Let (_, bs, a) -> is_value a && List.for_all is_value_binding bs
| Struct xs -> List.for_all is_value xs
| Var _ -> false
| Int _ | Bool _ | Function _ -> true

and is_value_binding (_, x) = is_value x

and is_value x = is_value_t' x.t

(* FIXME Add environments to make closures *)
let mkt x = {t = x; lets = []}

let string_of_t e = "None"

let print_env_item (n, e) =
  Printf.printf "%s: %s\n" n (string_of_t e)

let print_env env =
  List.iter print_env_item env

(* Eval-in-one-go. Implicit lets not really relevant here, because we just put
them in the env. *)
let rec eval env e =
  match e.t with
    Int _ | Bool _ | Function (_, _) -> e
  | Var v ->
      begin try eval env (List.assoc v env) with
        _ ->
         Printf.printf "Looking for %s\n" v;
         print_env env;
         raise Exit
      end
  | IfThenElse ({t = Bool b}, x, y) -> eval env (if b then x else y)
  | IfThenElse (c, x, y) -> eval env {e with t = IfThenElse (eval env c, x, y)}
  | Times (x, y) ->
      begin match (eval env x).t, (eval env y).t with
        Int x, Int y -> mkt (Int (x * y))
      | _ -> failwith "eval-times"
      end
  | Minus (x, y) ->
      begin match (eval env x).t, (eval env y).t with
        Int x, Int y -> mkt (Int (x - y))
      | _ -> failwith "eval-minus"
      end
  | Equals (x, y) ->
      begin match (eval env x).t, (eval env y).t with
        Int x, Int y -> mkt (Bool (x = y))
      | _ -> failwith "eval-equals"
      end
  | Apply ({t = Function (v, b)}, y) ->
      eval ((v, eval env y)::env) b
  | Apply (f, y) ->
      eval env {e with t = Apply (eval env f, eval env y)}
  | Let (recflag, bindings, e) ->
      let env' =
        List.map
          (fun (n, be) ->
             let benv =
               if recflag
                 then bindings @ env
                 else env
             in
               (n, eval benv be))
          bindings
      in
        eval env' e
  | Struct es ->
      mkt (Struct (List.map (eval env) es))

(* let rec factorial x = if x = 0 then 1 else x * factorial (x - 1) in factorial 4 *)
let factorial =
  mkt (Struct [
    mkt (Let (true,
         [("factorial",
           mkt (Function ("x", mkt (IfThenElse (mkt (Equals (mkt (Var "x"), mkt (Int 0))),
                       mkt (Int 1),
                       mkt (Times (mkt (Var "x"),
                                   mkt (Apply (mkt (Var "factorial"), mkt (Minus
                                   (mkt (Var "x"), mkt (Int 1))))))))))))],
         (mkt (Apply (mkt (Var "factorial"), mkt (Int 4))))))
      ])

(* let a = 6
 * let f x = a
 * let a = 7
 * let y = f 0 *)
let closures =
  Struct [mkt (LetDef (false, [("a", mkt (Int 6))]));
          mkt (LetDef (false, [("f", mkt (Function ("x", mkt (Var "a"))))]));
          mkt (LetDef (false, [("a", mkt (Int 7))]));
          mkt (LetDef (false, [("y", mkt (Apply (mkt (Var "f"), mkt (Int 0))))]))]
let _ =
  if not !Sys.interactive then ignore (eval [] factorial)

