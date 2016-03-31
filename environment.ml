(* Uses the tiny-ocaml simple AST *)
open Tinyocaml
open Evalutils

type t = Tinyocaml.t

let calc = TinyocamlUtils.calc

let comp = TinyocamlUtils.comp

let is_value = TinyocamlUtils.is_value

(* True if a variable appears not occluded by a let. *)
let rec appears var = function
  Var v when v = var -> true
| Op (_, a, b) | And (a, b) | Or (a, b) | Cmp (_, a, b) | App (a, b)
| Seq (a, b) -> appears var a || appears var b
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
| Fun {fname; fexp} -> fname <> var && appears var fexp
| Record items ->
    List.exists (fun (_, {contents = e}) -> appears var e) items
| Field (e, n) -> appears var e
| SetField (e, n, e') -> appears var e || appears var e'
| TryWith (e, (s, e')) -> appears var e || appears var e'
| CallBuiltIn (_, args, _) -> List.exists (appears var) args
| Module ls -> List.exists (appears var) ls
| Int _ | Bool _ | Var _ | Float _ | Unit
| Raise _ | OutChannel _ | InChannel _ | String _ -> false

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

exception ExceptionRaised of string

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
            Fun ({fname; fexp; fper} as f) ->
              if fper then last := InsidePervasive::!last;
              if fname = n then e else Fun {f with fexp = Let (n, v, fexp)}
          | _ -> failwith "should not be here / eval Let (n, v, e)"
        else e
      else
        Let (n, v, eval peek ((n, v)::env) e)
    else
      Let (n, eval peek env v, e)
| LetRec (n, (Fun r as f), e) ->
    if r.fper || namestarred n then last := InsidePervasive::!last;
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
| App (Fun ({fname; fexp; fper} as f), x) ->
    if fper then last := InsidePervasive::!last;
    if is_value x
      then Let (fname, x, fexp)
      else App (Fun f, eval peek env x)
| App (Var v, x) ->
    begin match List.assoc v env with
      Fun {fname; fexp; fper} ->
        if fper then last := InsidePervasive::!last;
        if is_value x then Let (fname, x, fexp) else App (Var v, eval peek env x)
    | exception Not_found ->
        eval peek env (App (List.assoc v Core.core, x))
    | _ -> failwith "Malformed app"
    end
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
| Field (Record items, n) -> !(List.assoc n items)
| Field (e, n) -> Field (eval peek env e, n)
| SetField (Record items, n, e) ->
    if is_value e
      then (if not peek then (List.assoc n items) := e; Unit)
      else SetField (Record items, n, eval peek env e)
| SetField (e, n, e') ->
    SetField (eval peek env e, n, e')
| Raise e ->
    raise (ExceptionRaised e)
| TryWith (e, (s, e')) ->
    if is_value e then e else
      begin try eval peek env e with
        ExceptionRaised x when x = s ->
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
| Int _ | Bool _ | Float _ | Fun _ | Unit | OutChannel _ | InChannel _ | String _ -> failwith "already a value"

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
      else Next (collect_unused_lets (eval false Core.core e))
  with
    ExceptionRaised s ->
      Printf.printf "Exception reached top level: %s\n" s;
      IsValue
  | x ->
      Printf.printf "Error in environment %s\n" (Printexc.to_string x);
      Malformed "environment"

let tree x =
  makestructure (Tinyocaml.to_real_ocaml x)

let to_string x =
  Pptinyocaml.to_string (TinyocamlUtils.underline_redex x) 

let tiny x = TinyocamlUtils.underline_redex x

let peek x =
  if is_value x then [] else
    let t = !last in
      last := [];
      ignore (eval true Core.core x);
      let r = !last in
        last := t;
        r

let last x = !last

