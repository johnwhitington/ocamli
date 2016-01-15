open Tinyocaml

type assoc = L | R | N

let assoc = function
  Int _ | Bool _ | Var _ | If _ | Let _ | LetRec _ | Fun _ -> N
| Op _ | Cmp _ | App _ -> L
| And _ | Or _ -> R

let prec = function
    App _ -> 100
  | Op ((Mul | Div), _, _) -> 90
  | Op (_, _, _) -> 80
  | Cmp _ -> 70
  | And _ | Or _ -> 60
  | If _ -> 50
  | Fun _ | Let _ | LetRec _ -> 10
  | _ -> max_int

let unopt = function
  Some x -> x
| None -> failwith "unopt"

let parens node parent isleft =
  match parent with
    None -> ("","")
  | Some p ->
      if
           prec node > prec p
        || assoc node = L && isleft && assoc p = L && prec node = prec p
        || assoc node = R && not isleft && assoc p = R && prec node = prec p
      then
        ("","")
      else
        ("(", ")")

let rec string_of_tiny_inner isleft parent node =
  match node with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Var v -> v
  | Op (op, l, r) ->
      let lp, rp = parens node parent isleft in
        Printf.sprintf "%s%s %s %s%s"
          lp (string_of_tiny_inner true (Some node) l)
          (Tinyocaml.string_of_op op)
          (string_of_tiny_inner false (Some node) r) rp
  | _ -> failwith "unimplemented"

let string_of_tiny =
  string_of_tiny_inner true None
