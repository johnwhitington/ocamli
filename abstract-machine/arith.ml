type op = Add | Sub | Mul | Div

type prog =
  Int of int
| Op of prog * op * prog

type assoc = L | N | R

let assoc = function
  Op _ -> L
| Int _ -> N

let prec = function
  Int _ -> max_int
| Op (_, (Mul | Div), _) -> 100
| Op (_, _, _) -> 50

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

type instr =
  IConst of int
| IOp of op

let rec compile = function
  Int i -> [IConst i]
| Op (a1, op, a2) -> compile a1 @ compile a2 @ [IOp op]

let calc_op a b = function
  Add -> a + b
| Sub -> a - b
| Mul -> a * b
| Div -> a / b (* May cause Division_by_zero *)

let rec uncompile s = function
  [] -> begin match s with e::_ -> e | _ -> failwith "uncompile: stack" end
| IConst i::r -> uncompile (Int i::s) r
| IOp op::r ->
   match s with
     a::b::s' -> uncompile (Op (b, op, a)::s') r
   | _ -> failwith "uncompile: stack empty"

let string_of_op = function
  Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

let rec print isleft parent node =
  let lp, rp = parens node parent isleft in
  match node with
    Int i ->
      Printf.sprintf "%i" i
  | Op (a, op, b) ->
      Printf.sprintf
        "%s%s %s %s%s"
        lp
        (print true (Some node) a)
        (string_of_op op)
        (print true (Some node) b)
        rp

let print x =
  print false None x

let rec run s = function
  [] ->
    begin match s with x::_ -> x | _ -> failwith "run: stack empty" end
| IConst i::r -> run (i::s) r
| IOp op::r ->
    begin match s with
      n2::n1::s' -> run ((calc_op n1 n2 op)::s') r
    | _ -> failwith "run: stack empty 2"
    end

let example_prog =
  Op (Int 5, Mul, Op (Int 1, Add, Int 2))

let example_instrs =
  compile example_prog

let _ = run [] example_instrs 

