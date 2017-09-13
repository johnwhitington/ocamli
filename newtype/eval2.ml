open Nanocaml

let real_op = function
  Mul -> ( * )
| Div -> ( / )
| Add -> ( + )
| Sub -> ( - )

(* Eval-in-one-go. Implicit lets not really relevant here, because we just put
them in the env. *)
let rec eval (env : environment) e =
  Printf.printf "eval: %s\n" (Nanocaml.string_of_t e);
  Printf.printf "env:\n"; Nanocaml.print_env env;
  (*let env = add_implicit_lets_to_environment env (List.rev e.lets) in*)
    match e.t with
      Int _ | Bool _ | Function (_, _, _) -> e
    | Var v ->
        begin try eval env (lookup_in_environment v env) with
          _ ->
           Printf.printf "Looking for %s\n" v;
           print_env env;
           raise Exit
        end
    | Op (op, x, y) ->
        begin match (eval env x).t, (eval env y).t with
          Int x, Int y -> mkt (Int (real_op op x y))
        | _ -> failwith "eval-minus"
        end
    | Apply ({t = Function (fvar, fenv, b)}, y) ->
        let new_envitem = envitem_of_bindings false [(fvar, eval env y)] in
          eval (new_envitem :: (*fenv @*) env) b
    | Apply (f, y) ->
        eval env {e with t = Apply (eval env f, y)}
    | Struct es ->
        {e with t = Struct (eval_many env es)}

    (*| If ({t = Bool b}, x, y) -> eval env (if b then x else y)
    | If (c, x, y) -> eval env {e with t = If (eval env c, x, y)}*)

    (*| Equals (x, y) ->
        begin match (eval env x).t, (eval env y).t with
          Int x, Int y -> mkt (Bool (x = y))
        | _ -> failwith "eval-equals"
        end*)

    (*| Let (recflag, bindings, e) ->
        let env' =
           envitem_of_bindings recflag
             (List.map
               (fun (n, be) ->
                  let benv =
                    if recflag
                      then envitem_of_bindings recflag bindings :: env
                      else env
                  in
                    (n, eval benv be))
               bindings)
           :: env
        in
          eval env' e
    | LetDef (recflag, bindings) ->
        let benv =
          if recflag then envitem_of_bindings recflag bindings :: env else env
        in
          {e with t =
            LetDef (recflag, List.map (fun (n, be) -> (n, eval benv be)) bindings)}*)


and eval_many env = function
  [] -> []
| [e] -> [eval env e]
| {t = LetDef (recflag, bindings)} as e::es ->
    let benv = envitem_of_bindings recflag bindings :: env in
      eval (if recflag then benv else env) e :: eval_many benv es
| _ -> failwith "malformed struct: first not a letdef"

(* Eval one step, assuming not already a value *)
let rec seval (env : environment) e =
  (*Printf.printf "seval:: %s\n" (string_of_t' e.t);*)
  let env = add_implicit_lets_to_environment env (List.rev e.lets) in
    match e.t with
    | Var v ->
        begin try lookup_in_environment v env with
          _ ->
           Printf.printf "Looking for %s\n" v;
           print_env env;
           raise Exit
        end
    | If ({t = Bool true}, x, _) | If ({t = Bool false}, _, x) ->
        {x with lets = e.lets @ x.lets}
    | If (c, x, y) -> {e with t = If (seval env c, x, y)}
    | Op (op, {t = Int a}, {t = Int b}) -> {e with t = Int (real_op op a b)}
    | Op (op, ({t = Int _} as a), b) -> {e with t = Op (op, a, seval env b)}
    | Op (op, a, b) -> {e with t = Op (op, seval env a, b)}
    | Equals ({t = Int a}, {t = Int b}) -> {e with t = Bool (a = b)}
    | Equals ({t = Bool a}, {t = Bool b}) -> {e with t = Bool (a = b)}
    | Equals ({t = Int _ | Bool _} as a, b) -> {e with t = Equals (a, seval env b)}
    | Equals (a, b) -> {e with t = Equals (seval env a, b)}
    | Let (recflag, bindings, e') ->
        let new_bindings = seval_first_non_value_binding env bindings in
          if List.for_all is_value_binding new_bindings then
            {lets = (recflag, new_bindings)::e.lets @ e'.lets;
             t = e'.t}
          else
            {e with t = Let (recflag, new_bindings, e')}
    | Apply ({t = Function (v, fenv, b)} as f, x) ->
        if is_value x then
          let new_envitem = envitem_of_bindings false [(v, x)] in
            seval (new_envitem :: fenv @ env) b
        else
          let new_let = (false, [(v, x)]) in
            {e with t =
              Apply ({f with lets = new_let::e.lets}, seval env x)}
    | Apply (f, x) -> {e with t = Apply (seval env f, x)}
    | Struct items -> {e with t = Struct (seval_first_non_value env items)}
    | LetDef _ -> failwith "LetDef not in a struct"
    | _ ->
        failwith (Printf.sprintf "seval: %s already a value" (string_of_t e))

and seval_first_non_value env = function
  [] -> []
| {t = LetDef (recflag, bindings)} as h::more ->
     let env = add_implicit_lets_to_environment env (List.rev h.lets) in
       if is_value h then
         h::
           seval_first_non_value
             (envitem_of_bindings recflag bindings :: env)
             more
       else
         {h with t =
           LetDef (recflag, seval_first_non_value_binding env bindings)}::more
| h::t when is_value h -> h::seval_first_non_value env t
| h::t -> seval env h::t

and seval_first_non_value_binding env = function
  [] -> []
| (n, e)::t ->
    if is_value e
      then (n, e)::seval_first_non_value_binding env t
      else (n, seval env e)::t

