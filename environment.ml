(* Uses the tiny-ocaml simple AST *)
open Tinyocaml
open Evalutils

let bold, ul, code_end = ("\x1b[1m", "\x1b[4m", "\x1b[0m")

type t = Tinyocaml.t

let calc = TinyocamlUtils.calc

let comp = TinyocamlUtils.comp

(* Predicate on value-ness of expressions. Var _ is here for convenience *)
let is_value = TinyocamlUtils.is_value

(* Substitute a value for a name in an expresion *)
let substitute = TinyocamlUtils.substitute

(* If not a value, underline it as the next redex *)
let underline x =
  Control (ul, x, code_end)

exception UnderlineValueUnderLets

let rec underline_redex e =
  match e with
    Control (l, x, r) -> Control (l, underline_redex x, r)
  | Op (_, Int _, Int _) -> underline e
  | Op (op, Int a, b) -> Op (op, Int a, underline b)
  | Op (op, a, b) -> Op (op, underline a, b)
  | And (Bool false, _) -> underline e
  | And (Bool true, Bool _) -> underline e
  | And (Bool true, b) -> And (Bool true, underline b)
  | And (a, b) -> And (underline a, b)
  | Or (Bool true, _) -> underline e
  | Or (Bool false, Bool b) -> underline e
  | Or (Bool false, b) -> Or (Bool false, underline b)
  | Cmp (_, Int _, Int _) -> underline e
  | Cmp (op, Int a, b) -> Cmp (op, Int a, underline b)
  | Cmp (op, a, b) -> Cmp (op, underline a, b)
  | If (Bool _, _, _) -> underline e
  | If (cond, a, b) -> If (underline cond, a, b)
  | Let (n, v, e') ->
      if is_value v
        then Let (n, v, underline_redex e')
        else Let (n, underline v, e')
  | LetRec (n, Fun (var, body), e) -> underline e
  | LetRec (n, v, e') ->
      if is_value v then underline e else Let (n, underline v, e')
  | App (Fun f, x) ->
      if is_value x then underline e else App (Fun f, underline x)
  | App (f, x) -> App (underline f, x)
  | Var _ -> underline e
  | _ -> raise UnderlineValueUnderLets

let underline_redex e =
  if is_value e then e else
    try underline_redex e with
      UnderlineValueUnderLets -> underline e

exception ValueUnderLets of Tinyocaml.t

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
    (* If a value, add to the environment, retain, and continue search for
    redex.  Otherwise, evaluate its rhs one step *)
    if is_value v
    then
      (Hashtbl.add env n v; Let (n, v, eval env e))
    else
      Let (n, eval env v, e)
| LetRec (n, Fun (var, body), e) ->
    let v = Fun (var, LetRec (n, Fun (var, body), body)) in
      substitute n v e
| LetRec (n, v, e) ->
    if is_value v then substitute n v e else LetRec (n, eval env v, e)
| App (Fun (n, body) as f, x) ->
    if is_value x
      then Let (n, x, body) (*substitute n x body*)
      else App (f, eval env x)
| App (f, x) -> App (eval env f, x)
| Var v ->
    begin try Hashtbl.find env v with Not_found -> failwith "Var not found" end
| (Int _ | Bool _ | Fun _) as e ->
    (* Value possibly under value-lets. Shed lets. *)
    raise (ValueUnderLets e)
 
let init x = Tinyocaml.of_real_ocaml (getexpr x)

let next e =
  try
    if is_value e then IsValue else Next (eval (Hashtbl.create 100) e) 
  with
    ValueUnderLets e -> Next e
  | x ->
      Printf.printf "Error in environment %s\n" (Printexc.to_string x);
      Malformed "environment"

let tree x = makestructure (Tinyocaml.to_real_ocaml x)

let to_string x = Pptinyocaml.string_of_tiny (underline_redex x) 
