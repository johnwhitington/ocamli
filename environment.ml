(* Uses the tiny-ocaml simple AST *)
open Tinyocaml
open Evalutils

type t = Tinyocaml.t

let calc = TinyocamlUtils.calc

let comp = TinyocamlUtils.comp

let is_value = TinyocamlUtils.is_value

let fastcurry = ref false

let dopeek = ref true

let rec bound_in_pattern = function
  PatAny -> []
| PatVar v -> [v]
| PatTuple ls -> List.flatten (List.map bound_in_pattern ls)
| PatNil -> []
| PatCons (h, t) -> bound_in_pattern h @ bound_in_pattern t
| PatAlias (a, p) -> a::bound_in_pattern p

(* True if a variable appears not occluded by a let. *)
let rec appears var = function
  Var v when v = var -> true
| Op (_, a, b) | And (a, b) | Or (a, b) | Cmp (_, a, b) | App (a, b)
| Seq (a, b) | Cons (a, b) | Append (a, b) -> appears var a || appears var b
| While (a, b, c, d) ->
    appears var a || appears var b || appears var c || appears var d
| For (v, a, flag, b, c, copy) ->
    appears var a || appears var b || appears var c || appears var copy
| If (a, b, c) -> appears var a || appears var b || appears var c
| Control (_, x) -> appears var x
| Let (recflag, bindings, e') ->
    if recflag then
      List.exists
        (fun (PatVar v, e) -> v <> var && (appears var e || appears var e'))
        bindings
    else
      List.exists
        (fun (PatVar v, e) -> appears var e || v <> var && appears var e')
        bindings
| LetDef (recflag, bindings) ->
    if recflag then
      List.exists (fun (PatVar v, e) -> v <> var && appears var e) bindings
    else
      List.exists (fun (_, e) -> appears var e) bindings
| Match (e, patmatch) ->
    appears var e || List.exists (appears_in_case var) patmatch
| Fun (fname, fexp) -> fname <> var && appears var fexp
| Record items ->
    List.exists (fun (_, {contents = e}) -> appears var e) items
| Field (e, n) -> appears var e
| SetField (e, n, e') -> appears var e || appears var e'
| TryWith (e, (s, e')) -> appears var e || appears var e'
| CallBuiltIn (_, args, _) -> List.exists (appears var) args
| Struct ls -> List.exists (appears var) ls
| Tuple ls -> List.exists (appears var) ls
| Raise (_, Some x) -> appears var x
| Raise (_, None) -> false
| Int _ | Bool _ | Var _ | Float _ | Unit
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

let rec collect_unused_lets = function
  Let (recflag, bindings, e) ->
    if
      List.for_all (fun (_, v) -> is_value v) bindings &&
      not (List.exists (function (PatVar n, _) -> appears n e | _ -> false) bindings)
    then
      collect_unused_lets e
    else
      Let
        (recflag,
         List.map (fun (n, e) -> (n, collect_unused_lets e)) bindings,
         collect_unused_lets e)
| x -> Tinyocaml.recurse collect_unused_lets x

(* Evaluate one step, assuming not already a value *)
let lookup_int_var env v =
  match List.assoc v env with
    Int i -> i
  | _ -> failwith "comparison not an integer"

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
    | Int i, PatInt i' when i = i' -> yes
    | e, PatVar v -> Some (Let (false, [(PatVar v, e)], rhs))
    | Nil, PatNil -> yes
    | Cons (h, t), PatCons (ph, pt) ->
        begin match matches h ph rhs with
        | Some rhs' -> matches t pt rhs'
        | None -> no
        end
    | Tuple es, PatTuple ps ->
        match_many_binders es ps rhs
    | e, PatAlias (a, p) ->
        matches e p (Let (false, [(PatVar a, e)], rhs))
    | _ -> no

and match_many_binders es ps rhs =
  match es, ps with
    [], [] -> Some rhs
  | eh::et, ph::pt ->
      begin match matches eh ph rhs with
        None -> None
      | Some rhs' -> match_many_binders et pt rhs'
      end
  | _ -> failwith "match_many_binders"

let rec eval peek env expr =
  match expr with
| Control (_, x) -> eval peek env x
| Op (op, Int a, Int b) ->
    last := Arith::!last;
    Int (calc op a b)
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
| Or (a, b) -> And (eval peek env a, b)
| Cmp (op, Int a, Int b) ->
    last := Comparison::!last;
    Bool (comp op a b)
| Cmp (op, Var a, Int b) ->
    last := Comparison::!last;
    Bool (comp op (lookup_int_var env a) b)
| Cmp (op, Int a, Var b) ->
    last := Comparison::!last;
    Bool (comp op a (lookup_int_var env b))
| Cmp (op, Var a, Var b) ->
    last := Comparison::!last;
    Bool (comp op (lookup_int_var env a) (lookup_int_var env b))
| Cmp (op, Int a, b) -> Cmp (op, Int a, eval peek env b)
| Cmp (op, a, b) -> Cmp (op, eval peek env a, b)
| If (Bool true, a, _) -> a
| If (Bool false, _, b) -> b
| If (cond, a, b) -> If (eval peek env cond, a, b)
| Let (false, bindings, e) ->
    if List.exists (function (PatVar v, e) -> namestarred v | _ -> false) bindings then
      last := InsidePervasive::!last;
    if List.for_all (fun (_, e) -> is_value e) bindings then
      let env' =
          Evalutils.option_map (function (PatVar v, e) -> Some (v, e) | _ -> None) bindings
        @ env
      in
        Let (false, bindings, eval peek env' e)
    else
      Let (false, eval_first_non_value_binding peek false env [] bindings, e)
| Let (true, [PatVar n, (Fun r as f)], e) ->
    if namestarred n then last := InsidePervasive::!last;
    if is_value e then e else
      Let (true, [PatVar n, f], eval peek ((n, f)::env) e)
| Let (true, _, _) -> failwith "malformed letrec"
| LetDef (recflag, bindings) ->
    if List.for_all (fun (_, e) -> is_value e) bindings
      then
        failwith "letdef already a value"
      else
        LetDef (recflag, eval_first_non_value_binding peek recflag env [] bindings)
        (*LetDef (recflag, [PatVar v, eval peek (if recflag then (v,e)::env else * env) e])*)
| App (Fun ((fname, fexp) as f), x) ->
    if is_value x
      then Let (false, [PatVar fname, x], fexp)
      else App (Fun f, eval peek env x)
| App (Function [], x) ->
    Raise ("Match_failure", Some (Tuple [String "FIXME"; Int 0; Int 0]))
| App (Function (p::ps), x) ->
    if is_value x
      then 
        begin match eval_case peek env x p with
        | Matched e -> e
        | EvaluatedGuardStep p' -> App (Function (p'::ps), x)
        | FailedToMatch -> App (Function ps, x)
        end
      else App (Function (p::ps), eval peek env x)
| App (Var v, x) ->
    begin match List.assoc v env with
      Fun (fname, fexp) ->
        if is_value x then
          Let (false, [PatVar fname, x], fexp)
        else
          App (Var v, eval peek env x)
    | _ -> failwith (Printf.sprintf "Malformed app (%s) (%s)" v (Tinyocaml.to_string x))
    end
| App (App _, _) when !fastcurry ->
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
(* If start number unevaluted, work on it *)
| For (v, e, ud, e', e'', copy) when not (is_value e) ->
    For (v, eval peek env e, ud, e', e'', copy)
(* If end number unevaluated, work on it *)
| For (v, e, ud, e', e'', copy) when not (is_value e') ->
    For (v, e, ud, eval peek env e', e'', copy)
(* If start number > end number (UP) or end number > start number (DOWN), we are done *)
| For (_, Int x, UpTo, Int y, _, _) when x > y -> Unit
| For (_, Int x, DownTo, Int y, _, _) when y > x -> Unit
(* If body a value, increment to next for loop *)
| For (v, Int x, ud, e', e'', copy) when is_value e'' ->
    For (v, Int (x + 1), ud, e', copy, copy)
(* If body not a value, put the var in the environment and evaluate body one step. *)
| For (v, x, ud, e', e'', copy) ->
    For (v, x, ud, e', eval peek ((v, x)::env) e'', copy)
| Record items ->
    eval_first_non_value_record_item peek env items;
    Record items
| Struct ls ->
    Struct (eval_first_non_value_item peek env [] ls)
| Tuple ls ->
    Tuple (eval_first_non_value_item peek env [] ls)
| Field (Record items, n) -> !(List.assoc n items)
| Field (e, n) -> Field (eval peek env e, n)
| SetField (Record items, n, e) ->
    if is_value e
      then (if not peek then (List.assoc n items) := e; Unit)
      else SetField (Record items, n, eval peek env e)
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
| TryWith (e, (s, e')) ->
    if is_value e then e else
      begin try eval peek env e with
        ExceptionRaised (x, payload) when x = s ->
          e'
      end
| CallBuiltIn (name, args, fn) ->
    if List.for_all is_value args
      then if not peek then fn args else Unit
      else CallBuiltIn (name, eval_first_non_value_item peek env [] args, fn)
| Var v ->
    begin try List.assoc v env with
      Not_found -> failwith (Printf.sprintf "Var %s not found" v)
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
      Match (eval peek env x, p::ps)
    else
      begin match eval_case peek env x p with
      | Matched e -> e
      | EvaluatedGuardStep p' -> Match (x, p'::ps)
      | FailedToMatch -> Match (x, ps)
      end
| Int _ | Bool _ | Float _ | Fun _ | Unit | OutChannel _
| InChannel _ | String _ | Nil | ExceptionDef _ -> failwith "already a value"

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
| _ -> failwith "eval_curry_findfun"

and eval_curry peek env e =
  let x, did = eval_curry_inner peek env e in
    if did then x else
      eval_curry_makelets (eval_curry_findfun e) (eval_curry_collect_args [] e)

and eval_curry_collect_args args = function
    App (f, e) -> eval_curry_collect_args (e::args) f
  | _ -> List.rev args

and eval_curry_makelets f args =
  match f, args with
  | Fun (a, fexp), [x] ->
      Let (false, [PatVar a, x], fexp)
  | Fun (a, fexp), x::xs ->
      Let (false, [PatVar a, x], eval_curry_makelets fexp xs)
  | _ -> failwith "eval_curry_makelets"

and eval_first_non_value_item peek env r = function
  [] -> List.rev r
| h::t ->
    if is_value h
      then
        let env' =
          match h with
            LetDef (_, [PatVar x, y]) -> (x, y)::env
          | _ -> env
        in
          eval_first_non_value_item peek env' (h::r) t
      else List.rev r @ [eval peek env h] @ t

and eval_first_non_value_binding peek recflag env r = function
  [] -> List.rev r
| (v, e)::t ->
    let env' =
      if recflag then
        match v with PatVar v -> (v, e)::env | _ -> env
      else env
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

let init x =
  Tinyocaml.of_real_ocaml x

let init_from_tinyocaml x = x

let next e =
  last := [];
  try
    if is_value e
      then IsValue
      else Next (collect_unused_lets (eval false Core.pervasives e))
  with
    ExceptionRaised (s, payload) -> raise (ExceptionRaised (s, payload))
  | x ->
      Printf.printf "Error in environment %s\n" (Printexc.to_string x);
      Malformed "environment"

let to_string x =
  Pptinyocaml.to_string (TinyocamlUtils.underline_redex x) 

let tiny x = TinyocamlUtils.underline_redex x

let peek x =
  if is_value x || not !dopeek then [] else
    let t = !last in
      last := [];
      ignore (eval true Core.pervasives x);
      let r = !last in
        last := t;
        r

let last x = !last

let newlines = function
  Struct (_::_::_) -> true
| _ -> false

