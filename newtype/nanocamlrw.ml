open Nanocaml

(* Convert from tinyocaml, building our own fenvs *)
let rec of_tinyocaml env = function
  Tinyocaml.Int i -> mkt (Int i)
| Tinyocaml.Bool b -> mkt (Bool b)
| Tinyocaml.Var v -> mkt (Var v)
| Tinyocaml.If (a, b, Some c) ->
    mkt (If (of_tinyocaml env a, of_tinyocaml env b, of_tinyocaml env c))
| Tinyocaml.Op (Tinyocaml.Mul, a, b) ->
    mkt (Op (Mul, of_tinyocaml env a, of_tinyocaml env b))
| Tinyocaml.Op (Tinyocaml.Sub, a, b) ->
    mkt (Op (Sub, of_tinyocaml env a, of_tinyocaml env b))
| Tinyocaml.Op (Tinyocaml.Add, a, b) ->
    mkt (Op (Add, of_tinyocaml env a, of_tinyocaml env b))
| Tinyocaml.Op (Tinyocaml.Div, a, b) ->
    mkt (Op (Div, of_tinyocaml env a, of_tinyocaml env b))
| Tinyocaml.Cmp (Tinyocaml.EQ, a, b) ->
    mkt (Equals (of_tinyocaml env a, of_tinyocaml env b))
| Tinyocaml.Let (recflag, bindings, e) ->
    let theref = ref [] in
    let env' = (recflag, theref)::env in
    theref := List.map (of_tinyocaml_binding env') bindings;
    let of_e = of_tinyocaml env' e in
    (* If all bindings are values, put this in as an implicit let. *)
    if List.for_all is_value_binding !theref then
      {t = of_e.t; lets = (recflag, !theref)::of_e.lets}
    else
      mkt (Let (recflag, !theref, of_e))
| Tinyocaml.App (a, b) -> mkt (Apply (of_tinyocaml env a, of_tinyocaml env b))
| Tinyocaml.Fun (_, PatVar v, e, fenv) ->
    mkt (Function (v, env, of_tinyocaml env e))
| Tinyocaml.Function ([(Tinyocaml.PatVar v, None, rhs)], fenv) ->
    mkt (Function (v, env, of_tinyocaml env rhs))
| Tinyocaml.Struct (_, es) ->
    mkt (Struct (of_tinyocaml_many env es))
| e -> failwith (Printf.sprintf "of_tinyocaml: unknown structure %s" (Tinyocaml.to_string e))

and of_tinyocaml_many env = function
  [] -> []
| Tinyocaml.LetDef (recflag, bindings)::es ->
    let theref = ref [] in
    let env' = (recflag, theref)::env in
    theref := List.map (of_tinyocaml_binding env') bindings;
    mkt (LetDef (recflag, !theref))::of_tinyocaml_many env' es
| e::es ->
    of_tinyocaml env e::of_tinyocaml_many env es

and of_tinyocaml_binding env = function
    PatVar v, t -> (v, of_tinyocaml env t)
  | PatAny, t -> ("_", of_tinyocaml env t)
  | _ -> failwith "unknown pattern in of_tinyocaml_binding"

let of_tinyocaml x = of_tinyocaml [] x

let rec to_tinyocaml e =
  match e.lets with
    (recflag, bindings)::t ->
      Tinyocaml.Let
        (recflag,
         List.map to_tinyocaml_binding bindings,
         to_tinyocaml {e with lets = t})
  | [] ->
      match e.t with
        Int i -> Tinyocaml.Int i
      | Bool b -> Tinyocaml.Bool b
      | Var v -> Tinyocaml.Var v
      | If (a, b, c) ->
          Tinyocaml.If (to_tinyocaml a, to_tinyocaml b, Some (to_tinyocaml c))
      | Op (Mul, a, b) ->
          Tinyocaml.Op (Tinyocaml.Mul, to_tinyocaml a, to_tinyocaml b)
      | Op (Sub, a, b) ->
          Tinyocaml.Op (Tinyocaml.Sub, to_tinyocaml a, to_tinyocaml b)
      | Op (Add, a, b) ->
          Tinyocaml.Op (Tinyocaml.Add, to_tinyocaml a, to_tinyocaml b)
      | Op (Div, a, b) ->
          Tinyocaml.Op (Tinyocaml.Div, to_tinyocaml a, to_tinyocaml b)
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
      | Struct es -> Tinyocaml.Struct (false, List.map to_tinyocaml es)

and to_tinyocaml_binding (v, t) =
  (PatVar v, to_tinyocaml t)

and to_tinyocaml_fenv envitems = [] (* Just used for printing. fenv doesn't matter *)

