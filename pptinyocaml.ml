open Tinyocaml

let bold, ul, code_end = ("\x1b[1m", "\x1b[4m", "\x1b[0m")

type assoc = L | R | N

let rec assoc = function
  Control (_, x) -> assoc x
| Op _ | Cmp _ | App _ -> L
| And _ | Or _ | Seq _ | SetField _ -> R
| _ -> N

let prec = function
  Field _ -> 110
| App _ -> 100
| Op ((Mul | Div), _, _) -> 90
| Op (_, _, _) -> 80
| Cmp _ -> 70
| And _ -> 65
| Or _ -> 60
| SetField _ -> 55
| If _ -> 50
| Fun _ | Let _ | LetRec _ -> 10
| _ -> max_int

let parens node parent isleft =
  match parent with
    None -> ("","")
  | Some p ->
      if   prec node > prec p
        || assoc node = N && prec node = prec p
        || assoc node = L && isleft && assoc p = L && prec node = prec p
        || assoc node = R && not isleft && assoc p = R && prec node = prec p
      then
        ("","")
      else
        ("(", ")")

let rec string_of_tiny_inner isleft parent node =
  let lp, rp = parens node parent isleft in
  match node with
  | Control (Underline, x) -> ul ^ string_of_tiny_inner isleft parent x ^ code_end
  | Control (Pervasive, _) -> ""
  | Unit -> "()"
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | String s -> "\"" ^ String.escaped s ^ "\""
  | OutChannel s -> "<out_channel>"
  | InChannel s -> "<in_channel>"
  | CallBuiltIn (args, fn) -> "<call_built_in>"
  | Var v -> v
  | Op (op, l, r) ->
      Printf.sprintf "%s%s %s %s%s"
        lp (string_of_tiny_inner true (Some node) l)
        (Tinyocaml.string_of_op op)
        (string_of_tiny_inner false (Some node) r) rp
  | Cmp (cmp, l, r) ->
      Printf.sprintf "%s%s %s %s%s"
        lp (string_of_tiny_inner true (Some node) l)
        (Tinyocaml.string_of_cmp cmp)
        (string_of_tiny_inner false (Some node) r) rp
  | And (l, r) ->
      Printf.sprintf "%s%s %s %s%s"
        lp (string_of_tiny_inner true (Some node) l)
        "&&"
        (string_of_tiny_inner false (Some node) r) rp
  | Or (l, r) ->
      Printf.sprintf "%s%s %s %s%s"
        lp (string_of_tiny_inner true (Some node) l)
        "||"
        (string_of_tiny_inner false (Some node) r) rp
  | If (e, e1, e2) ->
      Printf.sprintf "%sif %s then %s else %s%s"
        lp
        (string_of_tiny_inner false (Some node) e)
        (string_of_tiny_inner false (Some node) e1)
        (string_of_tiny_inner false (Some node) e2)
        rp
  | Let (v, e, e') ->
      Printf.sprintf "%slet %s = %s in %s%s"
        lp v
        (string_of_tiny_inner false (Some node) e)
        (string_of_tiny_inner false (Some node) e')
        rp
  | LetRec (v, e, e') ->
      Printf.sprintf "%slet rec %s = %s in %s%s"
        lp v
        (string_of_tiny_inner false (Some node) e)
        (string_of_tiny_inner false (Some node) e')
        rp
  | Fun (v, e) ->
      Printf.sprintf "%sfun %s -> %s%s"
        lp v (string_of_tiny_inner false (Some node) e) rp
  | App (e, e') ->
      Printf.sprintf "%s%s %s%s"
        lp
        (string_of_tiny_inner false (Some node) e)
        (string_of_tiny_inner false (Some node) e')
        rp
  | Seq (e, e') ->
      Printf.sprintf "%s%s; %s%s"
        lp
        (string_of_tiny_inner false (Some node) e)
        (string_of_tiny_inner false (Some node) e')
        rp
  | Record items ->
      Printf.sprintf "%s{%s}%s"
        lp
        (List.fold_left ( ^ ) "" (List.map string_of_record_entry items))
        rp
  | Field (e, n) ->
      Printf.sprintf "%s%s.%s%s"
        lp (string_of_tiny_inner false (Some node) e) n rp
  | SetField (e, n, e') ->
      Printf.sprintf "%s%s.%s <- %s%s"
        lp
        (string_of_tiny_inner false (Some node) e)
        n
        (string_of_tiny_inner false (Some node) e')
        rp
  | Raise e ->
      Printf.sprintf "%sraise %s%s" lp e rp
  | TryWith (e, (s, e')) ->
      Printf.sprintf "%stry %s with %s -> %s%s"
        lp
        (string_of_tiny_inner false (Some node) e)
        s
        (string_of_tiny_inner false (Some node) e')
        rp

and string_of_record_entry (n, {contents = e}) =
  Printf.sprintf "%s = %s" n (string_of_tiny_inner false None e)

let rec remove_named_recursive_functions fns = function
  LetRec (n, v, e) ->
    let r = Tinyocaml.recurse (remove_named_recursive_functions fns) e in
      if List.mem n fns then r else LetRec (n, v, r)
| x -> Tinyocaml.recurse (remove_named_recursive_functions fns) x

let string_of_tiny ?(remove_recs = []) tiny =
  let tiny =
    remove_named_recursive_functions remove_recs tiny
  in
    string_of_tiny_inner true None tiny

