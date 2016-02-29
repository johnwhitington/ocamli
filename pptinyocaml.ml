open Tinyocaml

let bold, ul, code_end = ("\x1b[1m", "\x1b[4m", "\x1b[0m")

type assoc = L | R | N

let rec assoc = function
  Control (_, x, _) -> assoc x
| Int _ | Bool _ | Var _ | If _ | Let _ | LetRec _ | Fun _ -> N
| Op _ | Cmp _ | App _ -> L
| And _ | Or _ | Seq _ -> R

let prec = function
  App _ -> 100
| Op ((Mul | Div), _, _) -> 90
| Op (_, _, _) -> 80
| Cmp _ -> 70
| And _ -> 65
| Or _ -> 60
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
  match node with
  | Control (s, x, s') -> s ^ string_of_tiny_inner isleft parent x ^ s'
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Var v -> v
  | Op (op, l, r) ->
      let lp, rp = parens node parent isleft in
        Printf.sprintf "%s%s %s %s%s"
          lp (string_of_tiny_inner true (Some node) l)
          (Tinyocaml.string_of_op op)
          (string_of_tiny_inner false (Some node) r) rp
  | Cmp (cmp, l, r) ->
      let lp, rp = parens node parent isleft in
        Printf.sprintf "%s%s %s %s%s"
          lp (string_of_tiny_inner true (Some node) l)
          (Tinyocaml.string_of_cmp cmp)
          (string_of_tiny_inner false (Some node) r) rp
  | And (l, r) ->
      let lp, rp = parens node parent isleft in
        Printf.sprintf "%s%s %s %s%s"
          lp (string_of_tiny_inner true (Some node) l)
          "&&"
          (string_of_tiny_inner false (Some node) r) rp
  | Or (l, r) ->
      let lp, rp = parens node parent isleft in
        Printf.sprintf "%s%s %s %s%s"
          lp (string_of_tiny_inner true (Some node) l)
          "||"
          (string_of_tiny_inner false (Some node) r) rp
  | If (e, e1, e2) ->
      let lp, rp = parens node parent isleft in
        Printf.sprintf "%sif %s then %s else %s%s"
          lp
          (string_of_tiny_inner false (Some node) e)
          (string_of_tiny_inner false (Some node) e1)
          (string_of_tiny_inner false (Some node) e2)
          rp
  | Let (v, e, e') ->
      let lp, rp = parens node parent isleft in
        Printf.sprintf "%slet %s = %s in %s%s"
        lp v
        (string_of_tiny_inner false (Some node) e)
        (string_of_tiny_inner false (Some node) e')
        rp
  | LetRec (v, e, e') ->
      let lp, rp = parens node parent isleft in
        Printf.sprintf "%slet rec %s = %s in %s%s"
        lp v
        (string_of_tiny_inner false (Some node) e)
        (string_of_tiny_inner false (Some node) e')
        rp
  | Fun (v, e) ->
      let lp, rp = parens node parent isleft in
        Printf.sprintf "%sfun %s -> %s%s"
          lp v (string_of_tiny_inner false (Some node) e) rp
  | App (e, e') ->
      let lp, rp = parens node parent isleft in
        Printf.sprintf "%s%s %s%s"
          lp
          (string_of_tiny_inner false (Some node) e)
          (string_of_tiny_inner false (Some node) e')
          rp
  | Seq (e, e') ->
      let lp, rp = parens node parent isleft in
        Printf.sprintf "%s%s; %s%s"
          lp
          (string_of_tiny_inner false (Some node) e)
          (string_of_tiny_inner false (Some node) e')
          rp

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

