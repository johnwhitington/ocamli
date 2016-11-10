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

(* Convert from tinyocaml, building our own fenvs *)
let rec of_tinyocaml env = function
  Tinyocaml.Int i -> mkt (Int i)
| Tinyocaml.Bool b -> mkt (Bool b)
| Tinyocaml.Var v -> mkt (Var v)
| Tinyocaml.If (a, b, None) -> failwith "no single-arm ifs please"
| Tinyocaml.If (a, b, Some c) ->
    mkt (If (of_tinyocaml env a, of_tinyocaml env b, of_tinyocaml env c))
| Tinyocaml.Op (Tinyocaml.Mul, a, b) ->
    mkt (Times (of_tinyocaml env a, of_tinyocaml env b))
| Tinyocaml.Op (Tinyocaml.Sub, a, b) ->
    mkt (Minus (of_tinyocaml env a, of_tinyocaml env b))
| Tinyocaml.Cmp (Tinyocaml.EQ, a, b) ->
    mkt (Equals (of_tinyocaml env a, of_tinyocaml env b))
| Tinyocaml.Let (recflag, bindings, e) ->
    (* FIXME extend env when calling of_tinyocaml_binding and of_tinyocaml. *)
    mkt (Let (recflag, List.map (of_tinyocaml_binding env) bindings, of_tinyocaml env e))
| Tinyocaml.LetDef (recflag, bindings) ->
    let env' =
      env
    in
      mkt (LetDef (recflag, List.map (of_tinyocaml_binding env') bindings))
| Tinyocaml.App (a, b) -> mkt (Apply (of_tinyocaml env a, of_tinyocaml env b))
| Tinyocaml.Fun (_, PatVar v, e, fenv) ->
    mkt (Function (v, env, of_tinyocaml env e))
| Tinyocaml.Function ([(Tinyocaml.PatVar v, None, rhs)], fenv) ->
    mkt (Function (v, env, of_tinyocaml env rhs))
| Tinyocaml.Struct (_, es) ->
    (* FIXME: Add each item to the env as we proceed downward *)
    mkt (Struct (List.map (of_tinyocaml env) es))
| e -> failwith (Printf.sprintf "of_tinyocaml: unknown structure %s" (Tinyocaml.to_string e))

and of_tinyocaml_binding env = function
  (PatVar v, t) -> (v, of_tinyocaml env t)
| _ -> failwith "unknown pattern in of_tinyocaml_binding"

let of_tinyocaml x = of_tinyocaml [] x

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
      Tinyocaml.Fun
        (Tinyocaml.NoLabel, Tinyocaml.PatVar v, to_tinyocaml e, to_tinyocaml_fenv fenv)
  | Struct es -> Tinyocaml.Struct (true, List.map to_tinyocaml es)

and to_tinyocaml_binding (v, t) =
  (PatVar v, to_tinyocaml t)

and to_tinyocaml_fenv envitems = [] (* Just used for printing. fenv doesn't matter *)

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

(* string -> ocaml ast -> tinyocaml ast -> newtype ast *)
let of_program_text s =
  of_tinyocaml (Tinyocamlrw.of_real_ocaml [] (Ocamliutil.ast s))

(* newtype ast -> tinyocaml ast -> pptinyocaml -> string *)
let to_program_text x =
  Pptinyocaml.to_string (to_tinyocaml x)

(* Run the program p *)
let run p = eval [] p

let show p =
  print_string (to_program_text p);
  print_string "\n"

let _ =
  match Sys.argv with
    [|_; filename|] ->
      let p = of_program_text (Ocamliutil.load_file filename) in
        show p;
        print_string "\n";
        show (run p)
  | _ ->
      prerr_string "Syntax: newtype <filename>\n";
      exit 2

let factorial =
  of_program_text
    "let rec factorial x =
       if x = 0 then 1 else x * factorial (x - 1)
     in
       factorial 4"

let closures =
  of_program_text
    "let a = 6
     let f x = a
     let a = 7
     let y = f 0"

(*let _ =
  print_string (to_program_text factorial);
  print_string "\n";
  print_string (to_program_text (eval [] factorial));
  print_string "\n\n"

let _ =
  print_string (to_program_text closures);
  print_string "\n";
  print_string (to_program_text (eval [] closures));
  print_string "\n\n"*)

(*(* let rec factorial x = if x = 0 then 1 else x * factorial (x - 1) in factorial 4 *)
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
               mkt (LetDef (false, [("y", mkt (Apply (mkt (Var "f"), mkt (Int
               0))))]))])*)

(*let _ =
  if not !Sys.interactive then ignore (eval [] closures)*)

