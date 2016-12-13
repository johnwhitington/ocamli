type op = Add | Sub | Mul | Div

type prog =
  Int of int
| Op of prog * op * prog

type assoc = L | N | R

type stack = prog list

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

let rec uncompile (s : stack) = function
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

let rec run (s : stack) = function
  [] ->
    begin match s with x::_ -> x | _ -> failwith "run: stack empty" end
| IConst i::r -> run (Int i::s) r
| IOp op::r ->
    begin match s with
      Int n2::Int n1::s' -> run (Int (calc_op n1 n2 op)::s') r
    | _ -> failwith "run: stack empty 2"
    end

let run_step (s : stack) = function
  [] ->
    begin match s with
      x::t -> ([], [x], false)
    | _ -> failwith "run_step: stack empty"
    end
| IConst i::r -> (r, Int i::s, false)
| IOp op::r ->
    begin match s with
      Int n2::Int n1::s' -> (r, Int (calc_op n1 n2 op)::s', true)
    | _ -> failwith "run_step: stack empty 2"
    end

let rec run_step_by_step (s : stack) p =
  let print () =
    print_string (print (uncompile s p));
    print_newline ()
  in
    match s, p with
      [Int x], [] -> print ()
    | _ ->
      let p, s, important = run_step s p in
        if important then print ();
        run_step_by_step s p

let example_prog =
  Op (Int 5, Mul, Op (Int 1, Add, Int 2))

let example_instrs =
  compile example_prog

let _ = run_step_by_step [] example_instrs 

