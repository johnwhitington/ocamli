(* Uses the tiny-ocaml simple AST *)
open Tinyocaml
open Ocamliutil
open Tinyocamlutil

type t = Tinyocaml.t

let debug = ref false

let fastcurry = ref false

let dopeek = ref true

let docollectunusedlets = ref true


(* True if a variable appears not shadowed. *)
let rec appears var = function
  Var v when v = var -> true
| Var _ -> false
| Open _ -> false
| LocalOpen (_, t) -> appears var t (* FIXME Do we need to take account of the open here? It alters names... *)
| Op (_, a, b) | And (a, b) | Or (a, b) | Cmp (_, a, b) | App (a, b)
| Seq (a, b) | Cons (a, b) | Append (a, b) -> appears var a || appears var b
| Constr (_, Some x) -> appears var x
| Constr (_, None) -> false
| While (a, b, c, d) ->
    appears var a || appears var b || appears var c || appears var d
| For (v, a, flag, b, c, copy) ->
    appears var a || appears var b || appears var c || appears var copy
| If (a, b, None) -> appears var a || appears var b
| If (a, b, Some c) -> appears var a || appears var b || appears var c
| Control (_, x) -> appears var x
| Annot (_, x, y) -> appears var x || appears var y
| Let (recflag, bindings, e') ->
    if recflag then
      (* Inside expression e', or in a rhs, all bindings in the letrec occlude *)
         (appears var e' || (List.exists (appears var) (List.map snd bindings)))
      && not (List.mem var (bound_in_bindings bindings)) 
    else
      (* If appears in rhs of a let or in (e' but not bound by the let) *)
         List.exists (appears var) (List.map snd bindings)
      || (appears var e' && not (List.mem var (bound_in_bindings bindings)))
| LetDef (recflag, bindings) ->
    (* If recursive, the bound names in all patterns occlude *)
    if recflag then
         not (List.mem var (bound_in_bindings bindings))
      && List.exists (fun (_, e) -> appears var e) bindings
    else
      List.exists (fun (_, e) -> appears var e) bindings
| Match (e, patmatch) ->
    appears var e || List.exists (appears_in_case var) patmatch
| Fun (flabel, fname, fexp, env) ->
    not (List.mem var (bound_in_pattern fname)) &&
    (appears var fexp || appears_in_label var flabel)
| Function (cases, env) -> List.exists (appears_in_case var) cases
| Record items ->
    List.exists (fun (_, {contents = e}) -> appears var e) items
| Field (e, n) -> appears var e
| SetField (e, n, e') -> appears var e || appears var e'
| TryWith (e, cases) -> appears var e || List.exists (appears_in_case var) cases
| CallBuiltIn (_, _, args, _) -> List.exists (appears var) args
| Struct (_, ls) -> List.exists (appears var) ls
| Tuple ls -> List.exists (appears var) ls
| Array items -> Array.exists (appears var) items
| Raise (_, Some x) -> appears var x
| Raise (_, None) -> false
| Assert x -> appears var x
| Lazy x -> appears var x
| Include x -> appears var x
| Functor (_, _, me) -> appears var me
| ModuleApply (_, x) -> appears var x
| Int _ | Bool _ | Float _ | Unit
| Int32 _ | Int64 _ | NativeInt _ | Char _ | TypeDef _ | Sig _
| ModuleBinding _ | ModuleConstraint _ | ModuleIdentifier _
| OutChannel _ | InChannel _ | String _ | Nil | ExceptionDef _ -> false

(* True if a) appears unoccluded in the 'when' expression b) appears unoccluded
in the rhs *)
and appears_in_case var (pat, guard, rhs) =
  let bound = bound_in_pattern pat in
    let appears_in_guard =
      match guard with
        None -> false
      | Some g -> appears var g
    in
      (appears_in_guard || appears var rhs) && not (List.mem var bound)

and appears_in_label var = function
  Optional (_, Some e) -> appears var e
| _ -> false

let any_appears vars e =
  List.exists (fun v -> appears v e) vars

(* Algorithm for pruning unused lets. This will go once new-lets is in. *)
let removals = ref 0

let rec collect_unused_lets_iter = function
  Let (recflag, bindings, e) ->
    if
      let a =
        List.for_all (fun (_, v) -> is_value v) bindings
      and b =
        (let all_names_bound =
          List.flatten (List.map bound_in_pattern (List.map fst bindings))
        in
          List.exists
            (fun n -> appears n e) all_names_bound)
      in
       a && not b
    then
        begin
          removals := !removals + 1;
          collect_unused_lets_iter e
        end
    else
      begin
        Let
          (recflag,
           List.map (fun (n, e) -> (n, collect_unused_lets_iter e)) bindings,
           collect_unused_lets_iter e)
      end
| x -> Tinyocaml.recurse collect_unused_lets_iter x

let rec collect_unused_lets x =
  removals := 0;
  let x' = collect_unused_lets_iter x in
    if !removals > 0 then collect_unused_lets x' else x'

(* The environment has type (bool * binding list) list. We return the first
 * binding found for the name, or raise Not_found *)
let rec lookup_value_in_binding v b =
  match b with
    PatVar v', x when v = v' -> Some x
  | PatTuple ps, Tuple vs ->
      lookup_value_in_bindings v
        (List.map2 (fun p v -> (p, v)) ps vs)
  | PatArray ps, Array vs ->
      lookup_value_in_bindings v
        (Array.to_list (Array.map2 (fun p v -> (p, v)) ps vs))
  | PatConstraint (p, _), data -> lookup_value_in_binding v (p, data)
  (*| PatCons (p, p), Cons (vv, vv') ->
  | PatAlias (n, p), v ->
  | PatOr (p, p'), v ->
  | PatConstr (n, None) ->
  | PatConstr (n, Some p) -> *)
  | _ -> (*if !debug then Printf.printf "*A%s" v;*) None

and lookup_value_in_bindings v = function
   [] -> (*if !debug then Printf.printf "*B%s" v;*) None
 | b::bs ->
     match lookup_value_in_binding v b with
       Some x -> Some x
     | None -> lookup_value_in_bindings v bs

let rec lookup_value v = function
  [] -> if !debug then Printf.printf "*C%s\n" v; None
| EnvBinding (_, bs)::t ->
    begin match lookup_value_in_bindings v !bs with
      Some x -> Some x
    | None -> lookup_value v t
    end
| EnvFunctor (n, input_module_name, modtype, e, env)::t ->
    if n = v
      then Some (Functor (input_module_name, modtype, e)) (* FIXME env *)
      else lookup_value v t

(* Evaluate one step, assuming not already a value *)
let lookup_int_var env v =
  match lookup_value v env with
    Some (Int i) -> i
  | Some e ->
      Printf.printf "It is %s\n" (Tinyocaml.to_string e);
      failwith "comparison not an integer"
  | None ->
      failwith "lookup_int_var None"

(* The last operation to have been done *)
let last = ref []

exception ExceptionRaised of string * Tinyocaml.t option

let rec append_values x y =
  match x with
    Nil -> y
  | Cons (a, b) -> Cons (a, append_values b y)
  | _ -> failwith "bad append"

type patmatchresult =
  Matched of t
| EvaluatedGuardStep of case
| FailedToMatch

let rec matches expr pattern rhs =
  let yes = Some rhs in
  let no = None in
    match expr, pattern with
      _, PatAny -> yes
    | x, PatConstraint (p, _) -> matches x p rhs
    | Unit, PatUnit -> yes
    | Bool b, PatBool b' when b = b' -> yes
    | Int i, PatInt i' when i = i' -> yes
    | Int32 i, PatInt32 i' when i = i' -> yes
    | Int64 i, PatInt64 i' when i = i' -> yes
    | NativeInt i, PatNativeInt i' when i = i' -> yes
    | String s, PatString s' when s = s' -> yes
    | Char c, PatChar c' when c = c' -> yes
    | Char x, PatCharRange (c, c') when x >= c && x <= c' -> yes 
    | e, PatVar v -> Some (Let (false, [(PatVar v, e)], rhs))
    | Nil, PatNil -> yes
    | Cons (h, t), PatCons (ph, pt) ->
        begin match matches h ph rhs with
        | Some rhs' -> matches t pt rhs'
        | None -> no
        end
    | Tuple es, PatTuple ps ->
        match_many_binders es ps rhs
    | Record es, PatRecord (_, ps) ->
        match_many_binders (List.map (!) (List.map snd es)) (List.map snd ps) rhs
    | Array es, PatArray ps ->
        match_many_binders (Array.to_list es) (Array.to_list ps) rhs
    | e, PatAlias (a, p) ->
        matches e p (Let (false, [(PatVar a, e)], rhs))
    | e, PatOr (a, b) ->
        begin match matches e a rhs with
          Some _ -> yes
        | _ -> matches e b rhs
        end
    | Constr (y, None), PatConstr (x, None) when x = y -> yes
    | Constr (y, Some yp), PatConstr (x, Some xp)
        when x = y -> matches yp xp rhs
    | _ -> no

and match_many_binders es ps rhs =
  match es, ps with
    [], [] -> Some rhs
  | eh::et, ph::pt ->
      begin match matches eh ph rhs with
        None -> None
      | Some rhs' -> match_many_binders et pt rhs'
      end
  | _ -> None

(* Remove any binding in [bs] which binds a variable found in [pat]. FIXME this
whole thing is wrong. e.g let x, y, z = ... should go to let x, y, _ if we
must remove z. This will all go away when we deal properly with the lets
problem? *)
let filter_bindings (pat : pattern) (bs : binding list) : binding list =
  Ocamliutil.option_map
    (function
     | (PatVar x, v) -> if List.mem x (bound_in_pattern pat) then None else Some (PatVar x, v)
     | _ -> None)
    bs

(* Put all the items in fenv as lets around the expression [e] *)
let build_lets_from_fenv (fenv : Tinyocaml.env) e =
  List.fold_left
    (fun e envitem ->
      match envitem with
        EnvBinding (rf, bs) ->
          if any_appears (bound_in_bindings !bs) e then
            Let (rf, !bs, e)
          else
            e
      | EnvFunctor (n, input_module_name, modtype, e', env) ->
          if appears n e then 
            (* Make a let with the functor. *)
            let binding =
              (PatVar n, Functor (input_module_name, modtype, e')) (* FIXME env?  *)
            in
              Let (false, [binding], e)
          else e)
    e
    fenv
  
let rec eval peek (env : Tinyocaml.env) expr =
  (*Printf.printf "env %i\n%!" (List.length env);*)
  (*Printf.printf "EVAL: %s\n\n" (Tinyocaml.to_string expr);*)
  match expr with
| Lazy e -> Lazy (eval peek env e)
| LocalOpen (n, e) -> LocalOpen (n, eval peek (open_module n env) e)
| Constr (n, Some x) -> Constr (n, Some (eval peek env x))
| Annot (n, x, y) -> Annot (n, x, eval peek env y)
| Assert (Bool false) ->
    Raise ("Assert_failure", Some (Tuple [String "//unknown//"; Int 0; Int 0]))
| Assert (Bool true) -> Unit
| Assert e -> Assert (eval peek env e)
| Control (_, x) -> eval peek env x
| Op (op, Int a, Int b) ->
    last := Arith::!last;
    begin try Int (calc op a b) with
      Division_by_zero -> Raise ("Division_by_zero", None)
    end
| Op (op, Int a, b) -> Op (op, Int a, eval peek env b)
| Op (op, a, b) -> Op (op, eval peek env a, b)
| And (Bool false, _) ->
    last := Boolean::!last;
    Bool false
| And (Bool true, Bool b) ->
    last := Boolean::!last;
    Bool b
| And (Bool true, b) -> eval peek env b
| And (a, b) -> And (eval peek env a, b)
| Or (Bool true, _) ->
    last := Boolean::!last;
    Bool true
| Or (Bool false, Bool b) ->
    last := Boolean::!last;
    Bool b
| Or (Bool false, b) -> eval peek env b
| Or (a, b) -> Or (eval peek env a, b)
| Cmp (op, Int a, Int b) ->
    last := Comparison::!last;
    Bool (comp op a b)
| Cmp (op, Var a, Int b) ->
    last := VarLookup::Comparison::!last;
    Bool (comp op (lookup_int_var env a) b)
| Cmp (op, Int a, Var b) ->
    last := VarLookup::Comparison::!last;
    Bool (comp op a (lookup_int_var env b))
| Cmp (op, Var a, Var b) ->
    last := VarLookup::Comparison::!last;
    Bool (comp op (lookup_int_var env a) (lookup_int_var env b))
| Cmp (op, Int a, b) -> Cmp (op, Int a, eval peek env b)
| Cmp (op, a, b) when is_value a && is_value b -> Bool (comp op a b)
| Cmp (op, a, b) when is_value b -> Cmp (op, eval peek env a, b)
| Cmp (op, a, b) -> Cmp (op, a, eval peek env b)
| If (Bool true, a, _) -> last := IfBool::!last; a
| If (Bool false, _, None) -> last := IfBool::!last; Unit
| If (Bool false, _, Some b) -> last := IfBool::!last; b
| If (cond, a, b) -> If (eval peek env cond, a, b)
| Let (recflag, bindings, e) ->
    if List.exists (function (PatVar v, e) -> isstarred v | _ -> false) bindings then
      last := InsidePervasive::!last;
    if List.for_all (fun (_, e) -> is_value e) bindings then
      let env' = EnvBinding (recflag, ref bindings)::env in
        (* If e is a function closure, move this Let inside the function (unless
        it is occluded. FIXME: Mutually-recursive bindings with a name clash may
        break this. See commentary in programs/and.ml *)
        begin match e with
          Fun (flabel, fx, fe, fenv) ->
            begin match filter_bindings fx bindings with
              [] -> Fun (flabel, fx, fe, fenv)
            | bindings' -> Fun (flabel, fx, Let (recflag, bindings', fe), fenv)
            end
        | Function (cases, fenv) ->
            (* Put in the guard of any case where it appears unoccluded by the
             * pattern. Put in the rhs of any case where it appears unoccluded
             * by the pattern *)
             let add_to_case (pat, guard, rhs) =
               (* Filter the bindings to remove anything bound in the pattern *)
               (* If non-empty, add the let-binding to guard and rhs *)
               let bindings' =
                 List.fold_left
                   (fun a b -> filter_bindings (PatVar b) bindings) bindings (bound_in_pattern pat)
               in
               match bindings' with
                 [] -> (pat, guard, rhs)
               | l ->
                   let rhs' =
                     Let (recflag, bindings', rhs)
                   and guard' =
                     match guard with
                     | None -> None
                     | Some g -> Some (Let (recflag, bindings', g))
                   in
                     (pat, guard', rhs')
             in
               Function (List.map add_to_case cases, fenv)
        | _ -> Let (recflag, bindings, eval peek env' e)
        end
    else
      Let (recflag, eval_first_non_value_binding peek false env [] bindings, e)
| LetDef (recflag, bindings) ->
    if List.for_all (fun (_, e) -> is_value e) bindings
      then
        failwith "letdef already a value"
      else
        LetDef (recflag, eval_first_non_value_binding peek recflag env [] bindings)
| App (Fun ((flabel, fname, fexp, fenv) as f), x) ->
    if is_value x
      then build_lets_from_fenv fenv (Let (false, [fname, x], fexp))
      else App (Fun f, eval peek env x)
| App (Function ([], fenv), x) ->
    Raise ("Match_failure", Some (Tuple [String "FIXME"; Int 0; Int 0]))
| App (Function ((p::ps), fenv), x) ->
    if is_value x then 
      let p =
        (* add the closure bindings to guard and rhs if not occluded by particular pattern *)
        let (pat, guard, rhs) = p in
          let fenv' =
            Ocamliutil.option_map
              (fun envitem ->
                 match envitem with
                   EnvBinding (recflag, bindings) ->
                     begin match filter_bindings pat !bindings with
                       [] -> None
                     | bs -> Some (EnvBinding (recflag, bindings))
                     end
                 | EnvFunctor (n, input_module_name, modtype, e, env) -> None (*FIXME EnvFunctor*)
              )
              fenv
          in
            (pat,
             begin match guard with None -> None | Some g -> Some (build_lets_from_fenv fenv' g) end,
             build_lets_from_fenv fenv' rhs) 
      in
        begin match eval_case peek env x p with (* 1 *)
        | Matched e -> e
        | EvaluatedGuardStep p' -> App (Function ((p'::ps), fenv), x)
        | FailedToMatch -> App (Function (ps, fenv), x)
        end
    else
      App (Function ((p::ps), fenv), eval peek env x) (* 2 *)
| App (Var v, x) ->
    begin match lookup_value v env with
      Some (Fun (flabel, fname, fexp, fenv)) ->
        if is_value x then
          (* We must use fenv to build lets here. This will go away when we have
          implicit-lets in the Tinyocaml.t data type *)
          build_lets_from_fenv fenv (Let (false, [fname, x], fexp))
        else
          App (Var v, eval peek env x)
    | Some (Function cases) ->
        eval peek env (App (Function cases, x)) (* FIXME this is substitution *)
    | Some (Var v') ->
        App (Var v', x)
    | Some got ->
        Printf.printf "Malformed app applying %s\n to %s\n - got %s\n"
        v (Tinyocaml.to_string x) (Tinyocaml.to_string got);
        failwith "malformed app"
    | None -> failwith "malformed app None"
    end
| App (App _, _) when !fastcurry && suitable_for_curry expr ->
    (* 3. FIXME: closure-env-fastcurry *)
    eval_curry peek env expr
| App (f, x) -> App (eval peek env f, x)
| Seq (e, e') ->
    if is_value e then e' else Seq (eval peek env e, e')
| While (Bool false, _, _, _) ->
    Unit
| While (Bool true, e', cg, cb) when not (is_value e') ->
    While (Bool true, eval peek env e', cg, cb)
| While (Bool true, e', cg, cb) when is_value e' ->
    While (cg, cb, cg, cb)
| While (e, e', cg, cb) ->
    While (eval peek env e, e', cg, cb)
| For (v, e, ud, e', e'', copy) when not (is_value e) ->
    For (v, eval peek env e, ud, e', e'', copy)
| For (v, e, ud, e', e'', copy) when not (is_value e') ->
    For (v, e, ud, eval peek env e', e'', copy)
| For (_, Int x, UpTo, Int y, _, _) when x > y -> Unit
| For (_, Int x, DownTo, Int y, _, _) when y > x -> Unit
| For (v, Int x, ud, e', e'', copy) when is_value e'' ->
    For (v, Int (x + 1), ud, e', copy, copy)
| For (v, x, ud, e', e'', copy) ->
    For (v, x, ud, e', eval peek (EnvBinding (false, ref [(PatVar v, x)])::env) e'', copy)
| Record items ->
    eval_first_non_value_record_item peek env items;
    Record items
| Struct (b, ls) ->
    Struct (b, eval_first_non_value_item peek env [] ls)
| Tuple ls ->
    Tuple (eval_first_non_value_item peek env [] ls)
| Array items ->
    eval_first_non_value_item_array peek env items;
    Array (items)
| Field (Record items, n) -> !(List.assoc n items)
| Field (e, n) -> Field (eval peek env e, n)
| SetField (Record items, n, e) ->
    if is_value e
      then
        begin
          if not peek then
            begin
              let item = List.assoc n items in
               item := e
            end;
          Unit
        end
      else
        begin
          SetField (Record items, n, eval peek env e)
        end
| SetField (e, n, e') ->
    SetField (eval peek env e, n, e')
| Raise (e, payload) ->
    begin match payload with
      Some x when not (is_value x) ->
        Raise (e, Some (eval peek env x))
    | _ ->
      (* FIXME: Need to include info about the last stage here, since it gets elided *)
      raise (ExceptionRaised (e, payload))
    end
| CallBuiltIn (typ, name, args, fn) ->
    let really_coerce typ input =
      match typ, input with
        TypChar, Int x -> Char (Char.chr x)
      | TypInt, Char x -> Int (Char.code x)
      | _ -> failwith "really_coerce: unimplemented coercion"
    in
    let coerce typ result =
      match typ with
      | Some x -> really_coerce x result
      | None -> result
    in
    if List.for_all is_value args
      then if not peek then
        coerce typ (fn args)
      else Unit
      else CallBuiltIn (typ, name, eval_first_non_value_item peek env [] args, fn)
| Var v ->
    last := VarLookup::!last;
    begin match lookup_value v env with
      Some x -> x
    | None ->
        print_string (to_string_env env);
        failwith (Printf.sprintf "Var %s not found" v)
    end
| Cons (x, y) ->
    if is_value x then
      Cons (x, eval peek env y)
    else
      Cons (eval peek env x, y)
| Append (x, y) ->
    if is_value x && is_value y then
      append_values x y
    else if is_value x then Append (x, eval peek env y)
    else Append (eval peek env x, y)
| Match (_, []) ->
    Raise ("Match_failure", Some (Tuple [String "FIXME"; Int 0; Int 0]))
| Match (x, p::ps) ->
    if not (is_value x) then
      try Match (eval peek env x, p::ps) with
        ExceptionRaised (x, payload) ->
          (*Printf.printf "Caught Exception raised in body of a match. Checking \
          for exception cases...\n";*)
          let exc_cases =
             option_map
              (function (PatException p, guard, rhs) -> Some (p, guard, rhs) | _ -> None)
              (p::ps)
          in
            (*Printf.printf "Found %i candidate cases...\n" (List.length
             * exc_cases);*)
            match eval_match_exception peek env x payload exc_cases with
              FailedToMatch -> Raise (x, payload)
            | EvaluatedGuardStep _ -> failwith "guards on exceptions not supported yet"
            | Matched e' -> e'
    else
      begin match eval_case peek env x p with
      | Matched e -> e
      | EvaluatedGuardStep p' -> Match (x, p'::ps)
      | FailedToMatch -> Match (x, ps)
      end
| TryWith (e, cases) ->
    if is_value e then e else
      begin try TryWith (eval peek env e, cases) with
        ExceptionRaised (x, payload) ->
          match eval_match_exception peek env x payload cases with
            FailedToMatch -> Raise (x, payload)
          | EvaluatedGuardStep case -> failwith "guards on exception matching not supported yet"
          | Matched e' -> e'
      end
| ModuleBinding (n, x) ->
    ModuleBinding (n, eval peek env x)
| ModuleApply (n, Struct x) ->
    ModuleApply (n, eval peek env (Struct x))
| Int _ | Bool _ | Float _ | Fun _ | Unit | OutChannel _
| Int32 _ | Int64 _ | NativeInt _ | Char _
| InChannel _ | String _ | Nil | ExceptionDef _ | TypeDef _
| Constr (_, None)
| Function _ | Sig _ | ModuleConstraint _ | ModuleIdentifier _ | Open _ | Functor _
| ModuleApply _ | Include _ ->
    failwith ("already a value or unimplemented: " ^ (Pptinyocaml.to_string
    expr) ^ "\n\n" ^ (Tinyocaml.to_string expr))

(* e.g eval_match_exception [Failure] [Some (String "foo") [(pattern, guard, rhs)]] *)
and eval_match_exception peek env exnname exnpayload cases =
  match cases with
    [] -> FailedToMatch
  | c::cs ->
      let expr = Constr (exnname, exnpayload) in
        match eval_case peek env expr c with
          Matched rhs -> Matched rhs
        | EvaluatedGuardStep x -> EvaluatedGuardStep x
        | FailedToMatch -> eval_match_exception peek env exnname exnpayload cs

(*  Matched (match List.hd cases with (_, _, x) -> x)  *)

and eval_case peek env expr (pattern, guard, rhs) =
  match matches expr pattern rhs with
  | Some rhs' ->
    begin match guard with
      None -> Matched rhs'
    | Some (Bool false) -> FailedToMatch
    | Some (Bool true) -> Matched rhs'
    | Some x -> EvaluatedGuardStep (pattern, Some (eval peek env x), rhs')
    end
  | None -> FailedToMatch

(* Apply curried function appliation App (App (f, x), x') etc. *)
(* 1. If the function 'f' is not a value, evaluate one step *)
(* 2. If any argument is not a value, evaluate one step *)
(* 3. Otherwise, we have a function and some value-arguments, so build the lets *)
and eval_curry_inner peek env e =
  match e with
    App (App _ as f', x') ->
      if is_value x' then
        let f'', did = eval_curry_inner peek env f' in
          (App (f'', x'), did)
      else
        (App (f', eval peek env x'), true)
  | App (f, x) when not (is_value f) -> (App (eval peek env f, x), true)
  | App (f, x) when not (is_value x) -> (App (f, eval peek env x), true)
  | x -> (x, false)

and eval_curry_findfun = function
  App (App (e, _), _) -> eval_curry_findfun e
| App (f, _) -> f
| Fun f -> Fun f
| e -> failwith (Printf.sprintf "eval_curry_findfun: %s" (Tinyocaml.to_string e))

and eval_curry peek env e =
  let x, did = eval_curry_inner peek env e in
    if did then x else
      eval_curry_makelets (eval_curry_findfun e) (eval_curry_collect_args [] e)

and eval_curry_collect_args args = function
    App (f, e) -> eval_curry_collect_args (e::args) f
  | _ -> args

and eval_curry_makelets f args =
  match f, args with
  | Fun (label, a, fexp, fenv), [x] ->
      Let (false, [a, x], fexp)
  | Fun (label, a, fexp, fenv), x::xs ->
      Let (false, [a, x], eval_curry_makelets fexp xs)
  | _ -> failwith "eval_curry_makelets"

(* For now, we only allow fun -> fun -> fun chains to be fast-curried. Any
function (i.e pattern match) in the process spoils things. *)
and suitable_for_curry e =
  let rec count_apps = function
    App (e, _) -> let (a, b) = count_apps e in (a + 1, b)
  | e -> (0, e)
  in
    let rec all_funs (n, e) =
      n = 0 || match e with Fun (_, _, e', _) -> all_funs (n - 1, e') | _ -> false
    in
      all_funs (count_apps e)

(* Substitute all instances of src for tgt in varname *)
and substitute_name src tgt varname =
  let split_on_char sep s =
    let r = ref [] in
    let j = ref (String.length s) in
    for i = String.length s - 1 downto 0 do
      if s.[i] = sep then begin
        r := String.sub s (i + 1) (!j - i - 1) :: !r;
        j := i
      end
    done;
    String.sub s 0 !j :: !r
  in
    let bits = split_on_char '.' varname in
      let newbits = List.map (fun x -> if x = src then tgt else x) bits in
        String.concat "." newbits

(* Do the name substitution src -> target. Take care with shadowing. *)
and substitute_module src tgt = function
  | Var x -> Var (substitute_name src tgt x)
  | Struct (x, items) -> Struct (x, substitute_module_list src tgt items)
  | x -> Tinyocaml.recurse (substitute_module src tgt) x

and substitute_module_list (src : string) (tgt : string) (code : Tinyocaml.t list) =
  match code with
  | ModuleBinding (n, x)::t when n = src -> ModuleBinding (n, x)::t (* shadows.  We're done *)
  | h::t -> substitute_module src tgt h::substitute_module_list src tgt t
  | [] -> []

(* Add an anonymous struct [module_to_include] to the beginning of the given
 * module [m] as a new module under the name "FIXME" *)
and add_as_fixme struct_to_include m =
  let themod = ModuleBinding ("FIXME", m) in
    match m with
      Struct (x, items) -> Struct (x, themod::items)
    | _ -> failwith "add_as_fixme: not a struct"

(* Do the functor application Modf(Modx), yielding a module *)
and apply_functor (env : Tinyocaml.env) (modf : string) (modx : Tinyocaml.t) =
  match lookup_value modf env, modx with
  | Some (Functor (fn, _, e)), ModuleIdentifier xn ->
      substitute_module fn xn e
  | Some (Functor (fn, _, e)), Struct s ->
      substitute_module fn "FIXME" (add_as_fixme (Struct s) e)
  | Some f, x -> failwith "apply_functor: not a functor"
  | _ -> failwith "apply_functor: not found"

(* Add a functor defintion to the environment as (name, ModuleBinding
(input_module_name, thestruct). This is a bit of a hack to avoid special casing
functor definitions in the Tinyocaml.env data type. *)
and add_functor_definition name functr (env : Tinyocaml.env) =
  match functr with
    Functor (input_module_name, modtype, thestruct) ->
      EnvFunctor (name, input_module_name, modtype, thestruct, [])::env (*FIXME env*)
  | _ -> failwith "add_functor_definition"

and eval_first_non_value_item peek (env : env) r = function
  [] -> List.rev r
| ModuleBinding (name, ModuleIdentifier original) as h::t ->
    eval_first_non_value_item peek (alias_module original name env) (h::r) t
| ModuleBinding (name, (Functor (_, _, _) as functr)) as h::t ->
    eval_first_non_value_item peek (add_functor_definition name functr env) (h::r) t
| ModuleBinding (name, ModuleConstraint (_, Struct (x, items)))
| ModuleBinding (name, Struct (x, items)) as h::t ->
    if is_value (Struct (x, items)) then
      eval_first_non_value_item peek (open_struct_as_module name items env) (h::r) t
    else
      let newstruct = eval_first_non_value_item peek env [] [Struct (x, items)] in
        List.rev r @ [ModuleBinding (name, List.hd newstruct)] @ t
| ModuleBinding (name, ModuleApply (ModuleIdentifier modf, modx))::t ->
    List.rev r @ [ModuleBinding (name, apply_functor env modf modx)] @ t
| Open name as h::t ->
    eval_first_non_value_item peek (open_module name env) (h::r) t
| h::t ->
    if is_value h then
      let env' =
        match h with LetDef (rf, bs) -> EnvBinding (rf, ref bs)::env | _ -> env
      in
        eval_first_non_value_item peek env' (h::r) t
    else
      List.rev r @ [eval peek env h] @ t

and eval_first_non_value_item_array peek (env : env) items =
  let rec loop p =
    if p > Array.length items - 1 then ()
    else if is_value items.(p) then loop (p + 1)
    else items.(p) <- eval peek env items.(p)
  in
    loop 0

and eval_first_non_value_binding
  peek recflag (env : env) r (bs : binding list)
=
  match bs with
    [] -> List.rev r
  | (v, e)::t ->
      let env' =
        if recflag then EnvBinding (false, ref [(v, e)])::env else env
      in
        if is_value e then
          eval_first_non_value_binding peek recflag env' ((v, e)::r) t
        else
          List.rev r @ [(v, eval peek env' e)] @ t

and eval_first_non_value_record_item peek env items =
  try
    List.iter (fun (_, v) -> if not (is_value !v) && not peek then v := eval peek env !v) items
  with
    Exit -> ()

let lib = ref []

let init x = x

let init_from_tinyocaml x = x

external reraise : exn -> 'a = "%reraise"

let next e =
  last := [];
  try
    if is_value e
      then IsValue
      else Next ((if !docollectunusedlets then collect_unused_lets else (fun x ->x)) (eval false !lib e))
  with
    ExceptionRaised (s, payload) -> raise (ExceptionRaised (s, payload))
  | x ->
      Printf.printf "Error in Eval.next %s\n" (Printexc.to_string x);
      if !debug then reraise x;
      Malformed "environment"

let to_string x =
  Pptinyocaml.to_string (Tinyocamlutil.underline_redex x) 

let rec eval_until_value show peek env e =
  if is_value e then e else
    let e = collect_unused_lets e in
      if show then
        begin
          print_string "BEGINNING OF STAGE\n";
          print_string (to_string e);
          print_string "\n"
        end;
      eval_until_value show peek env (eval peek env e)

let tiny x = Tinyocamlutil.underline_redex x

let peek x =
  if is_value x || not !dopeek then [] else
    let t = !last in
      last := [];
      ignore (eval true !lib x);
      let r = !last in
        last := t;
        r

let last x = !last

let newlines = function
  Struct (_, _::_::_) -> true
| _ -> false

