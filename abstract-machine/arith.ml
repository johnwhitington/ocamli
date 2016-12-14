type op = Add | Sub | Mul | Div

type prog =
  Int of int
| Op of prog * op * prog
| Underline of prog

type assoc = L | N | R

type stack = prog list

let rec assoc = function
  Op _ -> L
| Int _ -> N
| Underline x -> assoc x

let rec prec = function
  Int _ -> max_int
| Op (_, (Mul | Div), _) -> 100
| Op (_, _, _) -> 50
| Underline x -> prec x

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
| Underline _ -> failwith "compile: underline"

let calc_op a b = function
  Add -> a + b
| Sub -> a - b
| Mul -> a * b
| Div -> a / b (* May cause Division_by_zero *)

let rec uncompile first (s : stack) = function
  [] -> begin match s with e::_ -> e | _ -> failwith "uncompile: stack" end
| IConst i::r -> uncompile false (Int i::s) r
| IOp op::r ->
   match s with
     a::b::s' ->
       let prog_op = Op (b, op, a) in
       let prog_op = if first then Underline prog_op else prog_op in
         uncompile false (prog_op::s') r
   | _ -> failwith "uncompile: stack empty"

let uncompile s p =
  uncompile true s p

let string_of_op = function
  Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

let bold, ul, code_end = ("\x1b[1m", "\x1b[4m", "\x1b[0m")

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
  | Underline x ->
      ul ^ print isleft parent x ^ code_end

let print x =
  print false None x

(* Print the stack and instruction list *) 
let string_of_stack s =
  List.fold_left
    (fun a b -> a ^ (if a = "" then "" else "; ") ^ b)
    ""
    (List.map print s)

let string_of_instr = function
  IConst i -> Printf.sprintf "IConst %i" i
| IOp op -> Printf.sprintf "IOp %s" (string_of_op op)

let string_of_instrs s =
  List.fold_left
    (fun a b -> a ^ (if a = "" then "" else "; ") ^ b)
    ""
    (List.map string_of_instr s)

let print_step (s : stack) (p : instr list) =
  Printf.sprintf "%s || %s\n" (string_of_instrs p) (string_of_stack s)

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

let rec run_step_by_step debug show_unimportant quiet (s : stack) p =
  let print () =
    print_string (print (uncompile s p));
    print_newline ()
  in
    match s, p with
      [Int x], [] ->
        if not quiet then print ()
    | _ ->
      let p', s', important = run_step s p in
        if debug && (important || show_unimportant) then print_string (print_step s p);
        if not quiet && (important || show_unimportant) then print ();
        run_step_by_step debug show_unimportant quiet s' p'

let rec of_tinyocaml = function
  Tinyocaml.Int i -> Int i
| Tinyocaml.Op (Tinyocaml.Mul, a, b) ->
    Op (of_tinyocaml a, Mul, of_tinyocaml b)
| Tinyocaml.Op (Tinyocaml.Sub, a, b) ->
    Op (of_tinyocaml a, Sub, of_tinyocaml b)
| Tinyocaml.Op (Tinyocaml.Add, a, b) ->
    Op (of_tinyocaml a, Add, of_tinyocaml b)
| Tinyocaml.Op (Tinyocaml.Div, a, b) ->
    Op (of_tinyocaml a, Div, of_tinyocaml b)
| Tinyocaml.Struct (_, [x]) -> of_tinyocaml x
| e -> failwith (Printf.sprintf "of_tinyocaml: unknown structure %s" (Tinyocaml.to_string e))

let program = ref ""

let debug = ref false

let show_unimportant = ref false

let setfile s = ()

let times = ref 1

let quiet = ref false

let argspec =
  [("-e", Arg.Set_string program, " Evaluate the program text given");
   ("-debug", Arg.Set debug, " Show the bytecode and stack at every point.");
   ("-show-unimportant", Arg.Set show_unimportant, " Show after every bytecode instruction, even when no change");
   ("-times", Arg.Set_int times, " Do it many times");
   ("-quiet", Arg.Set quiet, " Suppress output")]

let prog_of_string s =
  of_tinyocaml (Tinyocamlrw.of_real_ocaml [] (Ocamliutil.ast s))

let go () =
  let prog = compile (prog_of_string !program) in
    for _ = 1 to !times do run_step_by_step !debug !show_unimportant !quiet [] prog done

let _ =
  Arg.parse argspec setfile
    "Syntax: arith -e program [-debug] [-show-unimportant]\n";
  go ()
