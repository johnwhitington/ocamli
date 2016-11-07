type binding = string * t

and bindings = binding list

and t =
  {t : t';
   lets : (bool * bindings) list} (* The implicit value-lets around any expression *)

and t' =
  Int of int
| Bool of bool
| Var of string
| If of t * t * t
| Times of t * t
| Minus of t * t
| Equals of t * t
| Let of bool * binding list * t
| LetDef of bool * binding list
| Apply of t * t
| Function of string * binding list * t
| Struct of t list

let mkt x = {t = x; lets = []}

let rec of_tinyocaml = function
  Tinyocaml.Int i -> mkt (Int i)
| Tinyocaml.Bool b -> mkt (Bool b)
| Tinyocaml.Var v -> mkt (Var v)
| Tinyocaml.If (a, b, None) -> failwith "no single-arm ifs please"
| Tinyocaml.If (a, b, Some c) ->
    mkt (If (of_tinyocaml a, of_tinyocaml b, of_tinyocaml c))
| Tinyocaml.Op (Tinyocaml.Mul, a, b) ->
    mkt (Times (of_tinyocaml a, of_tinyocaml b))
| Tinyocaml.Op (Tinyocaml.Sub, a, b) ->
    mkt (Minus (of_tinyocaml a, of_tinyocaml b))
| Tinyocaml.Cmp (Tinyocaml.EQ, a, b) ->
    mkt (Equals (of_tinyocaml a, of_tinyocaml b))
| Tinyocaml.Let (recflag, bindings, e) ->
    mkt (Let (recflag, List.map of_tinyocaml_binding bindings, of_tinyocaml e))
| Tinyocaml.LetDef (recflag, bindings) ->
    mkt (LetDef (recflag, List.map of_tinyocaml_binding bindings))
| Tinyocaml.App (a, b) -> mkt (Apply (of_tinyocaml a, of_tinyocaml b))
| Tinyocaml.Function ([(Tinyocaml.PatVar v, None, rhs)], fenv) ->
    mkt (Function (v, of_tinyocaml_fenv fenv, of_tinyocaml rhs))
| Tinyocaml.Struct (_, es) -> mkt (Struct (List.map of_tinyocaml es))
| _ -> failwith "of_tinyocaml: unknown structure"

and of_tinyocaml_fenv = function
  EnvBinding (_, bs)::t -> List.map of_tinyocaml_binding !bs @ of_tinyocaml_fenv t
| h::t -> failwith "of_tinyocaml_fenv"
| [] -> []

and of_tinyocaml_binding = function
  (PatVar v, t) -> (v, of_tinyocaml t)
| _ -> failwith "unknown pattern in of_tinyocaml_binding"

let rec to_tinyocaml e =
  match e.t with
    Int i -> Tinyocaml.Int i
  | Bool b -> Tinyocaml.Bool b
  | Var v -> Tinyocaml.Var v
  | If (a, b, c) ->
      Tinyocaml.If (to_tinyocaml a, to_tinyocaml b, Some (to_tinyocaml c))
  | Times (a, b) ->
      Tinyocaml.Op (Tinyocaml.Mul, to_tinyocaml a, to_tinyocaml b)
  | Minus (a, b) ->
      Tinyocaml.Op (Tinyocaml.Sub, to_tinyocaml a, to_tinyocaml b)
  | Equals (a, b) ->
      Tinyocaml.Cmp (Tinyocaml.EQ, to_tinyocaml a, to_tinyocaml b)
  | Let (recflag, bindings, e) ->
      Tinyocaml.Let (recflag, List.map to_tinyocaml_binding bindings, to_tinyocaml e)
  | LetDef (recflag, bindings) ->
      Tinyocaml.LetDef (recflag, List.map to_tinyocaml_binding bindings)
  | Apply (a, b) ->
      Tinyocaml.App (to_tinyocaml a, to_tinyocaml b)
  | Function (v, fenv, e) ->
      Tinyocaml.Function
        ([(Tinyocaml.PatVar v, None, to_tinyocaml e)], to_tinyocaml_fenv fenv)
  | Struct es -> Tinyocaml.Struct (true, List.map to_tinyocaml es)

and to_tinyocaml_binding (v, t) =
  (PatVar v, to_tinyocaml t)

and to_tinyocaml_fenv envitems =
  List.map
    (fun (v, t) ->
      Tinyocaml.EnvBinding (true, ref [(Tinyocaml.PatVar v, to_tinyocaml t)]))
    envitems

let rec is_value_t' = function
  If (a, b, c) -> is_value a && is_value b && is_value c
| Times (a, b) | Minus (a, b) | Equals (a, b) | Apply (a, b) -> is_value a && is_value b
| Let (_, bs, a) -> is_value a && List.for_all is_value_binding bs
| LetDef (_, bs) -> List.for_all is_value_binding bs
| Struct xs -> List.for_all is_value xs
| Var _ -> false
| Int _ | Bool _ | Function _ -> true

and is_value_binding (_, x) = is_value x

and is_value x = is_value_t' x.t

let string_of_t e = "None"

let print_env_item (n, e) =
  Printf.printf "%s: %s\n" n (string_of_t e)

let print_env env =
  List.iter print_env_item env

(* Eval-in-one-go. Implicit lets not really relevant here, because we just put
them in the env. *)
let rec eval env e =
  match e.t with
    Int _ | Bool _ | Function (_, _, _) -> e
  | Var v ->
      begin try eval env (List.assoc v env) with
        _ ->
         Printf.printf "Looking for %s\n" v;
         print_env env;
         raise Exit
      end
  | If ({t = Bool b}, x, y) -> eval env (if b then x else y)
  | If (c, x, y) -> eval env {e with t = If (eval env c, x, y)}
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
  | Apply ({t = Function (v, fenv, b)}, y) ->
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
  | LetDef (recflag, bindings) ->
      let benv =
        if recflag then bindings @ env else env
      in
        {e with t =
          LetDef (recflag, List.map (fun (n, be) -> (n, eval benv be)) bindings)}
  | Struct es ->
      {e with t = Struct (eval_many env es)}

and eval_many env = function
  [] -> []
| [e] -> [eval env e]
| {t = LetDef (recflag, bindings)} as e::es ->
    let benv = bindings @ env in
      eval (if recflag then benv else env) e :: eval_many benv es
| _ -> failwith "malformed struct: first not a letdef"

(* let rec factorial x = if x = 0 then 1 else x * factorial (x - 1) in factorial 4 *)
let factorial =
  mkt (Struct [
    mkt (Let (true,
         [("factorial",
           mkt (Function ("x", [], mkt (If (mkt (Equals (mkt (Var "x"), mkt (Int 0))),
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
  mkt (Struct [mkt (LetDef (false, [("a", mkt (Int 6))]));
               mkt (LetDef (false, [("f", mkt (Function ("x", [], mkt (Var "a"))))]));
               mkt (LetDef (false, [("a", mkt (Int 7))]));
               mkt (LetDef (false, [("y", mkt (Apply (mkt (Var "f"), mkt (Int 0))))]))])

let _ =
  if not !Sys.interactive then ignore (eval [] closures)

