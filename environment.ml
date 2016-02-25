(* Uses the tiny-ocaml simple AST *)
open Tinyocaml
open Evalutils

type t = Tinyocaml.t

let calc = TinyocamlUtils.calc

let comp = TinyocamlUtils.comp

(* Predicate on value-ness of expressions. Var _ is here for convenience *)
let is_value = TinyocamlUtils.is_value

(* Substitute a value for a name in an expresion *)
let substitute = TinyocamlUtils.substitute

(* True if a variable appears not occluded by a let. *)
let rec appears var = function
  Var v when v = var -> true
| Op (_, a, b) | And (a, b) | Or (a, b) | Cmp (_, a, b) | App (a, b) ->
    appears var a || appears var b
| If (a, b, c) -> appears var a || appears var b || appears var c
| Control (_, x, _) -> appears var x
| Let (v, e, e') ->
    appears var e || v <> var && appears var e'
| LetRec (v, e, e') ->
    v <> var && (appears var e || appears var e')
| Fun (v, e) -> v <> var && appears var e
| Int _ | Bool _ | Var _ -> false

let rec collect_unused_lets = function
  Let (n, v, e) | LetRec (n, v, e) ->
    (* Must be a value: side effects *)
    if is_value v && not (appears n e)
      then collect_unused_lets e
      else Let (n, collect_unused_lets v, collect_unused_lets e)
| x -> Tinyocaml.recurse collect_unused_lets x

(* Evaluate one step, assuming not already a value *)
let rec eval env = function
| Control (_, x, _) -> eval env x
| Op (op, Int a, Int b) -> Int (calc op a b)
| Op (op, Int a, b) -> Op (op, Int a, eval env b)
| Op (op, a, b) -> Op (op, eval env a, b)
| And (Bool false, _) -> Bool false
| And (Bool true, Bool b) -> Bool b
| And (Bool true, b) -> eval env b
| And (a, b) -> And (eval env a, b)
| Or (Bool true, _) -> Bool true
| Or (Bool false, Bool b) -> Bool b
| Or (Bool false, b) -> eval env b
| Or (a, b) -> And (eval env a, b)
| Cmp (op, Int a, Int b) -> Bool (comp op a b)
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
        (Hashtbl.add env n v; Let (n, v, eval env e))
    else
      Let (n, eval env v, e)
| LetRec (n, Fun (var, body), e) ->
    if is_value e then e else
      begin
        Hashtbl.add env n (Fun (var, body));
        LetRec (n, Fun (var, body), eval env e)
      end
| App (Fun (n, body) as f, x) ->
    if is_value x
      then Let (n, x, body)
      else App (f, eval env x)
| App (Var v, x) ->
    begin match Hashtbl.find env v with
      Fun (n, body) ->
        if is_value x then
          Let (n, x, body)
        else
          App (Var v, eval env x)
    | _ -> failwith "maformed App"
    end
| App (f, x) -> App (eval env f, x)
| Var v ->
    begin try Hashtbl.find env v with Not_found -> failwith "Var not found" end
| Int _ | Bool _ | Fun _ -> failwith "already a value"
| _ -> failwith "malformed node"
 
let init x =
  Tinyocaml.of_real_ocaml (getexpr x)

let init_from_tinyocaml x = x

let next e =
  try
    if is_value e
      then IsValue
      else Next (collect_unused_lets (eval (Hashtbl.create 100) e))
  with
    x ->
      Printf.printf "Error in environment %s\n" (Printexc.to_string x);
      Malformed "environment"

let tree x =
  makestructure (Tinyocaml.to_real_ocaml x)

let to_string x =
  Pptinyocaml.string_of_tiny (TinyocamlUtils.underline_redex x) 
