(* Uses the tiny-ocaml simple AST *)
open Tinyocaml
open Evalutils

type t = Tinyocaml.t

let calc = TinyocamlUtils.calc

let comp = TinyocamlUtils.comp

let is_value = TinyocamlUtils.is_value

let fastcurry = ref false

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
| Let (v, e, e') ->
    appears var e || v <> var && appears var e'
| LetRec (v, e, e') ->
    v <> var && (appears var e || appears var e')
| LetDef (v, e) ->
    appears var e
| LetRecDef (v, e) ->
    v <> var && appears var e
| Fun (fname, fexp) -> fname <> var && appears var fexp
| Record items ->
    List.exists (fun (_, {contents = e}) -> appears var e) items
| Field (e, n) -> appears var e
| SetField (e, n, e') -> appears var e || appears var e'
| TryWith (e, (s, e')) -> appears var e || appears var e'
| CallBuiltIn (_, args, _) -> List.exists (appears var) args
| Module ls -> List.exists (appears var) ls
| Tuple ls -> List.exists (appears var) ls
| Int _ | Bool _ | Var _ | Float _ | Unit
| Raise _ | OutChannel _ | InChannel _ | String _ | Nil | ExceptionDef _ -> false

let rec collect_unused_lets = function
  Let (n, v, e) ->
    (* Must be a value: side effects *)
    if is_value v && not (appears n e)
      then collect_unused_lets e
      else Let (n, collect_unused_lets v, collect_unused_lets e)
| LetRec (n, v, e) ->
    (* Must be a value: side effects *)
    if is_value v && not (appears n e)
      then collect_unused_lets e
      else LetRec (n, collect_unused_lets v, collect_unused_lets e)
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
| Let (n, v, e) ->
    (* If v a value, see if e is a value. If it is, remove Let. Otherwise add
    to the environment, retain, and continue search for redex.  If v not a
    value, evaluate its rhs one step *)
    (* In fact, we must check to see if the value is **closed**. For example
         let x = 1 in fun y -> x + y
         fun y -> let x = 1 in x + y
     *)
    if namestarred n then last := InsidePervasive::!last;
    if is_value v then
      if is_value e then
        if appears n e then
          match e with
            Fun (fname, fexp) ->
              if fname = n then e else Fun (fname, Let (n, v, fexp))
          | _ -> failwith "should not be here / eval Let (n, v, e)"
        else e
      else
        Let (n, v, eval peek ((n, v)::env) e)
    else
      Let (n, eval peek env v, e)
| LetRec (n, (Fun r as f), e) ->
    if namestarred n then last := InsidePervasive::!last;
    if is_value e then e else
      LetRec (n, f, eval peek ((n, f)::env) e)
| LetRec _ -> failwith "malformed letrec"
| LetDef (v, e) ->
    if is_value e
      then failwith "letdef already a value"
      else LetDef (v, eval peek env e)
| LetRecDef (v, e) ->
    if is_value e
      then failwith "letrecdef already a value"
      else LetRecDef (v, eval peek ((v, e)::env) e)
| App (Fun ((fname, fexp) as f), x) ->
    if is_value x
      then Let (fname, x, fexp)
      else App (Fun f, eval peek env x)
| App (Var v, x) ->
    begin match List.assoc v env with
      Fun (fname, fexp) ->
        if is_value x then Let (fname, x, fexp) else App (Var v, eval peek env x)
    | exception Not_found ->
        eval peek env (App (List.assoc v Core.pervasives, x))
    | _ -> failwith "Malformed app"
    end
(* Two applications in a row for currying. e.g (f 1) 2. This will be extended to 'n' in a
row, of course. We must a) recognise the pattern b) turn each non-value argument
into a value and then c) apply all the arguments to the function at once. *)
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
| Module ls ->
    Module (eval_first_non_value_item peek env [] ls)
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
    raise (ExceptionRaised (e, payload))
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
| Int _ | Bool _ | Float _ | Fun _ | Unit | OutChannel _
| InChannel _ | String _ | Nil | ExceptionDef _ -> failwith "already a value"

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
      Let (a, x, fexp)
  | Fun (a, fexp), x::xs ->
      Let (a, x, eval_curry_makelets fexp xs)
  | _ -> failwith "eval_curry_makelets"

and eval_first_non_value_item peek env r = function
  [] -> List.rev r
| h::t ->
    if is_value h
      then
        let env' =
          match h with
            LetDef binding -> binding::env
          | LetRecDef binding -> binding::env
          | _ -> env
        in
          eval_first_non_value_item peek env' (h::r) t
      else List.rev r @ [eval peek env h] @ t

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
    ExceptionRaised (s, payload) ->
      Printf.printf "Exception reached top level: %s\n" s;
      IsValue
  | x ->
      Printf.printf "Error in environment %s\n" (Printexc.to_string x);
      Malformed "environment"

(*let tree x =
  makestructure (Tinyocaml.to_real_ocaml x)*)

let to_string x =
  Pptinyocaml.to_string (TinyocamlUtils.underline_redex x) 

let tiny x = TinyocamlUtils.underline_redex x

let peek x =
  if is_value x then [] else
    let t = !last in
      last := [];
      ignore (eval true Core.pervasives x);
      let r = !last in
        last := t;
        r

let last x = !last

let newlines = function
  Module (_::_::_) -> true
| _ -> false



