(* Uses the tiny-ocaml simple AST *)
open Tinyocaml
open Evalutils

type t = Tinyocaml.t

let calc = TinyocamlUtils.calc

let comp = TinyocamlUtils.comp

(* Predicate on value-ness of expressions. Var _ is here for convenience *)
let is_value = TinyocamlUtils.is_value

(* True if a variable appears not occluded by a let. *)
let rec appears var = function
  Var v when v = var -> true
| Op (_, a, b) | And (a, b) | Or (a, b) | Cmp (_, a, b) | App (a, b)
| Seq (a, b) ->
    appears var a || appears var b
| If (a, b, c) -> appears var a || appears var b || appears var c
| Control (_, x) -> appears var x
| Let (v, e, e') ->
    appears var e || v <> var && appears var e'
| LetRec (v, e, e') ->
    v <> var && (appears var e || appears var e')
| Fun (v, e) -> v <> var && appears var e
| Record items ->
    List.exists (fun (_, {contents = e}) -> appears var e) items
| Field (e, n) -> appears var e
| SetField (e, n, e') -> appears var e || appears var e'
| TryWith (e, (s, e')) -> appears var e || appears var e'
| CallBuiltIn (args, _) -> List.exists (appears var) args
| Module ls -> List.exists (appears var) ls
| Int _ | Bool _ | Var _ | Unit | Raise _ | OutChannel _ | InChannel _ | String _ -> false

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

let last = ref Unknown

exception ExceptionRaised of string

let rec eval ?(peek = false) env = function
| Control (_, x) -> eval env x
| Op (op, Int a, Int b) ->
    last := Arith;
    Int (calc op a b)
| Op (op, Int a, b) -> Op (op, Int a, eval env b)
| Op (op, a, b) -> Op (op, eval env a, b)
| And (Bool false, _) ->
    last := Boolean;
    Bool false
| And (Bool true, Bool b) ->
    last := Boolean;
    Bool b
| And (Bool true, b) -> eval env b
| And (a, b) -> And (eval env a, b)
| Or (Bool true, _) ->
    last := Boolean;
    Bool true
| Or (Bool false, Bool b) ->
    last := Boolean;
    Bool b
| Or (Bool false, b) -> eval env b
| Or (a, b) -> And (eval env a, b)
| Cmp (op, Int a, Int b) ->
    last := Comparison;
    Bool (comp op a b)
| Cmp (op, Var a, Int b) ->
    last := Comparison;
    Bool (comp op (lookup_int_var env a) b)
| Cmp (op, Int a, Var b) ->
    last := Comparison;
    Bool (comp op a (lookup_int_var env b))
| Cmp (op, Var a, Var b) ->
    last := Comparison;
    Bool (comp op (lookup_int_var env a) (lookup_int_var env b))
| Cmp (op, Int a, b) -> Cmp (op, Int a, eval env b)
| Cmp (op, a, b) -> Cmp (op, eval env a, b)
| If (Bool true, a, _) -> a
| If (Bool false, _, b) -> b
| If (cond, a, b) -> If (eval env cond, a, b)
| Let (n, v, e) ->
    (* If v a value, see if e is a value. If it is, remove Let. Otherwise add
    to the environment, retain, and continue search for redex.  If v not a
    value, evaluate its rhs one step *)
    (* In fact, we must check to see if the value is **closed**. For example
         let x = 1 in fun y -> x + y
         fun y -> let x = 1 in x + y
     *)
    if is_value v then
      if is_value e then
        if appears n e then
          match e with
            Fun (fv, body) ->
              if fv = n then e else Fun (fv, Let (n, v, body))
          | _ -> failwith "should not be here / eval Let (n, v, e)"
        else e
      else
        Let (n, v, eval ((n, v)::env) e)
    else
      Let (n, eval env v, e)
| LetRec (n, Fun (var, body), e) ->
    if is_value e then e else
      LetRec (n, Fun (var, body), eval ((n, Fun (var, body))::env) e)
| LetRec _ -> failwith "malformed letrec"
| App (Fun (n, body) as f, x) ->
    if is_value x
      then Let (n, x, body)
      else App (f, eval env x)
| App (Var v, x) ->
    begin match List.assoc v env with
      Fun (n, body) ->
        if is_value x then Let (n, x, body) else App (Var v, eval env x)
    | exception Not_found ->
        eval env (App (List.assoc v Core.core, x))
    | _ -> failwith "Malformed app"
    end
| App (f, x) -> App (eval env f, x)
| Seq (e, e') ->
    if is_value e then e' else Seq (eval env e, e')
| Record items ->
    eval_first_non_value_record_item env items;
    Record items
| Module ls ->
    Module (eval_first_non_value_item env [] ls)
| Field (Record items, n) -> !(List.assoc n items)
| Field (e, n) -> Field (eval env e, n)
| SetField (Record items, n, e) ->
    if is_value e
      then (if not peek then (List.assoc n items) := e; Unit)
      else SetField (Record items, n, eval env e)
| SetField (e, n, e') ->
    SetField (eval env e, n, e')
| Raise e ->
    raise (ExceptionRaised e)
| TryWith (e, (s, e')) ->
    if is_value e then e else
      begin try eval env e with
        ExceptionRaised x when x = s ->
          e'
      end
| CallBuiltIn (args, fn) ->
    if List.for_all is_value args
      then fn args
      else CallBuiltIn (eval_first_non_value_item env [] args, fn)
| Var v ->
    begin try List.assoc v env with Not_found -> failwith "Var not found" end
| Int _ | Bool _ | Fun _ | Unit | OutChannel _ | InChannel _ | String _ -> failwith "already a value"

and eval_first_non_value_item env r = function
  [] -> List.rev r
| h::t ->
    if is_value h
      then eval_first_non_value_item env (h::r) t
      else List.rev r @ [eval env h] @ t

and eval_first_non_value_record_item env items =
  try
    List.iter (fun (_, v) -> if not (is_value !v) then v := eval env !v) items
  with
    Exit -> ()

let init x =
  Tinyocaml.of_real_ocaml x

let init_from_tinyocaml x = x

let next e =
  last := Unknown;
  try
    if is_value e
      then IsValue
      else Next (collect_unused_lets (eval Core.core e))
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
  if is_value x then Unknown else
    let t = !last in
      last := Unknown;
      ignore (eval Core.core ~peek:true x);
      let r = !last in
        last := t;
        r

let last x = !last

