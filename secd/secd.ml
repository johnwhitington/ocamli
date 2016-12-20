type op = Add | Sub | Mul | Div

(* The main type is done with deBruijn indexes when read in. *)
type prog =
  Int of int
| Op of prog * op * prog
| Apply of prog * prog
| Lambda of prog
| Let of prog * prog
| VarAccess of int
| Underline of prog

type assoc = L | N | R

type stack = prog list

let rec assoc = function
  Op _ | Apply _ -> L
| Underline x -> assoc x
| _ -> N


let rec prec = function
  Apply _ -> 100
| Op (_, (Mul | Div), _) -> 90
| Op (_, _, _) -> 80
| Let _ -> 10
| Underline x -> prec x
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

type instr =
  IEmpty
| IConst of int
| IOp of op
| IAccess of int
| IClosure of instr list
| ILet
| IEndLet
| IApply
| IReturn

let rec compile = function
  Int i -> [IConst i]
| Op (a1, op, a2) -> compile a1 @ compile a2 @ [IOp op]
| Apply (a, b) -> compile a @ compile b @ [IApply]
| Let (a, b) -> compile a @ [ILet] @ compile b @ [IEndLet]
| VarAccess i -> [IAccess i]
| Lambda p -> [IClosure (compile p @ [IReturn])]
| Underline _ -> failwith "compile: underline"

let calc_op a b = function
  Add -> a + b
| Sub -> a - b
| Mul -> a * b
| Div -> a / b (* May cause Division_by_zero *)

let rec uncompile first (s : stack) = function
  [] -> begin match s with e::_ -> e | _ -> failwith "uncompile: stack" end
| IConst i::r -> uncompile false (Int i::s) r
| IEmpty::_ -> failwith "uncompile: empty"
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
  | Apply (a, b) ->
      Printf.sprintf "%s %s"
        (print true (Some node) a) (print false (Some node) b)
  | Lambda body ->
      Printf.sprintf "fun _ -> %s" (print isleft (Some node) body)
  | Let (e, e') ->
      Printf.sprintf
        "let _ = %s in %s"
        (print isleft (Some node) e)
        (print isleft (Some node) e')
  | VarAccess x ->
      Printf.sprintf "VAR%i" x
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

let rec string_of_instr = function
  IConst i -> Printf.sprintf "IConst %i" i
| IOp op -> Printf.sprintf "IOp %s" (string_of_op op)
| IEmpty -> Printf.sprintf "IEmpty"
| IAccess i -> Printf.sprintf "IAccess %i" i
| IClosure instrs -> Printf.sprintf "IClosure [%s]" (string_of_instrs instrs)
| ILet -> "ILet"
| IEndLet -> "IEndLet"
| IApply -> "IApply"
| IReturn -> "IReturn"

and string_of_instrs s =
  List.fold_left
    (fun a b -> a ^ (if a = "" then "" else "; ") ^ b)
    ""
    (List.map string_of_instr s)

let print_step (s : stack) (p : instr list) =
  Printf.sprintf "%s || %s\n" (string_of_instrs p) (string_of_stack s)

let rec run (s : stack) = function
  [] ->
    begin match s with x::_ -> x | _ -> failwith "run: stack empty" end
| IEmpty::_ -> failwith "run: empty"
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
| IEmpty::_ -> failwith "run_step: empty"
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

let rec find_debruijn_index n v = function
  v'::vs when v = v' -> n
| _::vs -> find_debruijn_index (n + 1) v vs
| [] -> failwith "find_debruijn_index"

let rec of_tinyocaml db = function
  Tinyocaml.Int i -> Int i
| Tinyocaml.Op (Tinyocaml.Mul, a, b) ->
    Op (of_tinyocaml db a, Mul, of_tinyocaml db b)
| Tinyocaml.Op (Tinyocaml.Sub, a, b) ->
    Op (of_tinyocaml db a, Sub, of_tinyocaml db b)
| Tinyocaml.Op (Tinyocaml.Add, a, b) ->
    Op (of_tinyocaml db a, Add, of_tinyocaml db b)
| Tinyocaml.Op (Tinyocaml.Div, a, b) ->
    Op (of_tinyocaml db a, Div, of_tinyocaml db b)
| Tinyocaml.App (a, b) ->
    Apply (of_tinyocaml db a, of_tinyocaml db b)
| Tinyocaml.Let (false, [(PatVar v, e)], e') ->
    Let (of_tinyocaml (v::db) e, of_tinyocaml (v::db) e')
| Tinyocaml.Var v ->
    VarAccess (find_debruijn_index 0 v db)
| Tinyocaml.Fun (Tinyocaml.NoLabel, PatVar v, e, _) ->
    Lambda (of_tinyocaml (v::db) e)
| Tinyocaml.Struct (_, [x]) -> of_tinyocaml db x
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
  of_tinyocaml [] (Tinyocamlrw.of_real_ocaml [] (Ocamliutil.ast s))

let go () =
  let prog = compile (prog_of_string !program) in
    for _ = 1 to !times do run_step_by_step !debug !show_unimportant !quiet [] prog done

let _ =
  Arg.parse argspec setfile
    "Syntax: arith -e program [-debug] [-show-unimportant]\n";
  go ()
