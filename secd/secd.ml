
let bold, ul, code_end = ("\x1b[1m", "\x1b[4m", "\x1b[0m")

type op = Add | Sub | Mul | Div

(* The main type is done with deBruijn indexes when read in. *)
type name = string

type prog =
  Int of int
| Bool of bool
| VarAccess of name * int
| Eq of prog * prog
| Op of prog * op * prog
| Apply of prog * prog
| Lambda of name * prog
| Let of name * prog * prog
| If of prog * prog * prog
| Underline of prog

type assoc = L | N | R

let rec assoc = function
  Op _ | Apply _ | Eq _ -> L
| Underline x -> assoc x
| _ -> N

let rec prec = function
  Apply _ -> 100
| Op (_, (Mul | Div), _) -> 90
| Op (_, _, _) -> 80
| Eq _ -> 70
| If _ -> 50
| Let _ | Lambda _ -> 10
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
| IBool of bool
| IOp of op
| IEq
| IAccess of name * int
| IClosure of name * instr list
| ILet of name
| IEndLet
| IApply
| IReturn
| IBranch

type code = instr list

type environment = int list

type closure = instr list * environment

type stackitem =
  StackInt of int
| StackBool of bool
| StackClosure of name * closure
| StackCode of instr list
| StackEnvironment of environment
| StackProgram of prog

type stack = stackitem list

let rec compile = function
  Int i -> [IConst i]
| Bool b -> [IBool b]
| Eq (a, b) -> compile a @ compile b @ [IEq]
| Op (a1, op, a2) -> compile a1 @ compile a2 @ [IOp op]
| Apply (a, b) -> compile a @ compile b @ [IApply]
| Let (name, a, b) -> compile a @ [ILet name] @ compile b @ [IEndLet]
| VarAccess (s, i) -> [IAccess (s, i)]
| Lambda (n, p) -> [IClosure (n, (compile p @ [IReturn]))]
| If (a, b, c) -> compile (Lambda ("", b)) @ compile (Lambda ("", c)) @ compile a @ [IBranch]
| Underline _ -> failwith "compile: underline"

let calc_op a b = function
  Add -> a + b
| Sub -> a - b
| Mul -> a * b
| Div -> a / b (* May cause Division_by_zero *)

let string_of_op = function
  Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

let rec print isleft parent node =
  let lp, rp = parens node parent isleft in
  match node with
    Int i ->
      Printf.sprintf "%i" i
  | Bool b ->
      Printf.sprintf "%b" b
  | Op (a, op, b) ->
      Printf.sprintf
        "%s%s %s %s%s"
        lp
        (print true (Some node) a)
        (string_of_op op)
        (print true (Some node) b)
        rp
  | Eq (a, b) ->
      Printf.sprintf
        "%s%s = %s%s"
        lp
        (print true (Some node) a)
        (print true (Some node) b)
        rp
  | Apply (a, b) ->
      Printf.sprintf "%s%s %s%s"
        lp (print true (Some node) a) (print false (Some node) b) rp
  | Lambda (name, body) ->
      Printf.sprintf "%sfun %s -> %s%s" lp name (print isleft (Some node) body) rp
  | If (a, b, c) ->
      Printf.sprintf
        "%sif %s then %s else %s%s"
        lp
        (print isleft (Some node) a)
        (print isleft (Some node) b)
        (print isleft (Some node) c)
        rp
  | Let (name, e, e') ->
      Printf.sprintf
        "%slet %s = %s in %s%s"
        lp
        name
        (print isleft (Some node) e)
        (print isleft (Some node) e')
        rp
  | VarAccess (n, x) ->
      Printf.sprintf "%s" n
  | Underline x ->
      ul ^ print isleft parent x ^ code_end

let print x =
  print false None x

(* Print the stack and instruction list *) 
let string_of_int_list is =
  List.fold_left (fun a b -> a ^ b) "" (List.map string_of_int is)

let rec string_of_environment e =
  Printf.sprintf "<<env: %s>>" (string_of_int_list e)

and string_of_closure (instrs, env) =
  Printf.sprintf
    "<<closure (ops = [%s]), env = %s>>"
    (string_of_instrs instrs)
    (string_of_environment env)

and string_of_stackitem = function
  StackInt i -> Printf.sprintf "StackInt %i" i
| StackBool b -> Printf.sprintf "StackBool %b" b
| StackCode c -> Printf.sprintf "StackCode <<%s>>" (string_of_instrs c)
| StackClosure (n, c) -> Printf.sprintf "StackClosure (%s -> %s)" n (string_of_closure c)
| StackEnvironment e -> Printf.sprintf "StackEnvironment %s" (string_of_environment e)
| StackProgram p -> Printf.sprintf "StackProgram %s" (print p)

and string_of_stack s =
  List.fold_left
    (fun a b -> a ^ (if a = "" then "" else "; ") ^ b)
    ""
    (List.map string_of_stackitem s)

and string_of_instr = function
  IConst i -> Printf.sprintf "IConst %i" i
| IBool b -> Printf.sprintf "IBool %b" b
| IOp op -> Printf.sprintf "IOp %s" (string_of_op op)
| IEmpty -> Printf.sprintf "IEmpty"
| IAccess (n, i) -> Printf.sprintf "IAccess %s at %i" n i
| IClosure (_, instrs) -> Printf.sprintf "IClosure [%s]" (string_of_instrs instrs)
| ILet n -> "ILet " ^ n
| IBranch -> "IBranch"
| IEndLet -> "IEndLet"
| IApply -> "IApply"
| IReturn -> "IReturn"
| IEq -> "IEq"

and string_of_instrs s =
  List.fold_left
    (fun a b -> a ^ (if a = "" then "" else "; ") ^ b)
    ""
    (List.map string_of_instr s)

let prog_of_stackitem = function
  StackInt i -> Int i
| StackBool b -> Bool b
| StackProgram p -> p
| _ -> failwith "prog_of_stackitem"

(* For now, uncompile only works on these things:
  * 1) Whole programs i.e lists of instructions compiled from a program
  * 3) Execution fragments beginning IOp, IApply
  * 4) The empty final-state program with the final result left in the stack. *)
let rec uncompile (s : stack) (u : stack) (c : code) =
  match c with
    [] -> 
      begin match s with
        si::_ -> prog_of_stackitem si
        | _ -> failwith (Printf.sprintf "uncompile: stack = %s" (string_of_stack s))
      end
  | IAccess (n, l)::c' ->
      uncompile (StackProgram (VarAccess (n, l))::s) u c'
  | IEndLet::c' -> uncompile s u c'
  | IConst i::r -> uncompile (StackInt i::s) u r
  | IBool b::r -> uncompile (StackBool b::s) u r
  | IEmpty::_ -> failwith "uncompile: empty"
  | IEq::r ->
      begin match s with
        a::b::s' ->
          let prog = StackProgram (Eq (prog_of_stackitem b, prog_of_stackitem a)) in
            uncompile (prog::s') u r
      | _ -> failwith "uncompile: eq empty"
      end
  | IOp op::r ->
     begin match s with
       a::b::s' ->
         let prog_op = StackProgram (Op (prog_of_stackitem b, op, prog_of_stackitem a)) in
           uncompile (prog_op::s') u r
     | _ -> failwith ("uncompile: stack empty: " ^ string_of_stack s)
     end
  | ILet n::c' ->
      (* [let x = a in b] has [a] at the top of the stack, and [b] as everything else *)
      begin match s with
        si::_ -> Let (n, prog_of_stackitem si, uncompile s (StackProgram (prog_of_stackitem si)::u) c')
      | _ -> failwith "ilet: stack empty"
      end
  | IClosure (n, closure)::c' ->
      uncompile (StackClosure (n, (closure, []))::s) u c'
  | IBranch::c' ->
      begin match s with
        cond::StackClosure (_, (thn, e))::StackClosure (_, (els, e'))::s' ->
          If (prog_of_stackitem cond,
              uncompile s' u thn,
              uncompile s' u els)
      | _ -> failwith ("IBranch: stack empty" ^ string_of_stack s)
      end
  | IApply::c' ->
      (* f x has x on stack, then f. Collect them and make program *)
      begin match s with
        StackProgram p::StackClosure (n, (c, _))::_ -> Apply (Lambda (n, uncompile [] [] c), p)
      | StackInt i::StackClosure (n, (c, _))::_ -> Apply (Lambda (n, uncompile [] [] c), Int i)
      | _ -> failwith ("iapply: stack empty: " ^ string_of_stack s)
      end
  | IReturn::c' -> uncompile s u c'

let uncompile s u c =
  (*Printf.printf
    "Begin uncompile: %s %s %s\n" (string_of_stack s) (string_of_stack u) (string_of_instrs c);*)
  uncompile s u c

let print_step (s : stack) (p : instr list) (e : environment) =
  Printf.sprintf "%s || %s || %s\n"
    (string_of_instrs p) (string_of_stack s) (string_of_environment e)

let run_step (s : stack) (e : environment) = function
  [] ->
    begin match s with
      x::t -> ([], e, [x], false, false)
    | _ -> failwith "run_step: stack empty"
    end
| IEmpty::_ -> failwith "run_step: empty"
| IConst i::c -> (c, e, StackInt i::s, false, false)
| IBool b::c -> (c, e, StackBool b::s, false, false)
| IEq::c ->
    begin match s with
      n2::n1::s' -> (c, e, StackBool (n2 = n1)::s', false, true)
    | _ -> failwith "run_step: stack empty eq"
    end
| IOp op::c ->
    begin match s with
      StackInt n2::StackInt n1::s' ->
        (c, e, StackInt (calc_op n1 n2 op)::s', false, true)
    | _ -> failwith "run_step: stack empty 2"
    end
| ILet _::c ->
    begin match s with
      StackInt v::s' -> (c, (v::e), s', false, false)
    | _ -> failwith "run_step: stack empty let"
    end
| IEndLet::c ->
    begin match e with
      _::es -> (c, es, s, false, false)
    | [] -> failwith "run_step: env empty endlet"
    end
| IReturn::c ->
    begin match s with
      StackInt v::StackCode c'::StackEnvironment e'::s' ->
        (c, e', StackInt v::s', false, false)
    | _ -> failwith "run_step: IReturn"
    end
| IApply::c ->
    begin match s with
      StackInt v::StackClosure (n, (c', e'))::s' ->
        (c', v::e', StackCode c::StackEnvironment e::s', false, false)
    | _ -> failwith "run_step: IApply"
    end
| IAccess (_, i)::c ->
    (c, e, StackInt(List.nth e (i - 1))::s, false, true)
| IClosure (n, c')::c ->
    (c, e, StackClosure (n, (c', e))::s, false, false)
| IBranch::c' ->
    begin match s with
      StackBool b::StackClosure (_, (thn, e))::StackClosure (_, (els, e'))::s' ->
        ((if b then thn else els),
         (if b then e else e'),
         StackCode c'::StackEnvironment e::s,
         false,
         true)
    | _ -> failwith "run_step: IBranch"
    end

let rec run_step_by_step first debug quiet (s : stack) (e : environment) p =
  let print s p =
    print_string (print (uncompile s [] p));
    print_newline ()
  in
    match p with
      [] ->
        if debug then print_string (print_step s p e);
    | _ ->
        if debug then print_string (print_step s p e);
        let p', e', s', important_before, important_after = run_step s e p in
          if not quiet && important_before || first then print s p;
          if not quiet && important_after then print s' p';
          run_step_by_step false debug quiet s' e' p'

let rec find_debruijn_index n v = function
  v'::vs when v = v' -> n
| _::vs -> find_debruijn_index (n + 1) v vs
| [] -> failwith "find_debruijn_index"

let rec of_tinyocaml db = function
  Tinyocaml.Int i -> Int i
| Tinyocaml.Bool b -> Bool b
| Tinyocaml.Cmp (Tinyocaml.EQ, a, b) ->
    Eq (of_tinyocaml db a, of_tinyocaml db b)
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
    Let (v, of_tinyocaml db e, of_tinyocaml (v::db) e')
| Tinyocaml.Var v ->
    VarAccess (v, find_debruijn_index 1 v db)
| Tinyocaml.Fun (Tinyocaml.NoLabel, PatVar v, e, _) ->
    Lambda (v, of_tinyocaml (v::db) e)
| Tinyocaml.If (a, b, Some c) ->
    If (of_tinyocaml db a, of_tinyocaml db b, of_tinyocaml db c)
| Tinyocaml.Struct (_, [x]) -> of_tinyocaml db x
| e -> failwith (Printf.sprintf "of_tinyocaml: unknown structure %s" (Tinyocaml.to_string e))

let program = ref ""

let debug = ref false

let setfile s =
  program := Ocamliutil.load_file s

let times = ref 1

let quiet = ref false

let argspec =
  [("-e", Arg.Set_string program, " Evaluate the program text given");
   ("-debug", Arg.Set debug, " Show the bytecode and stack at every point.");
   ("-times", Arg.Set_int times, " Do it many times");
   ("-quiet", Arg.Set quiet, " Suppress output")]

let prog_of_string s =
  of_tinyocaml [] (Tinyocamlrw.of_real_ocaml [] (Ocamliutil.ast s))

let go () =
  let prog = compile (prog_of_string !program) in
    for _ = 1 to !times do
      run_step_by_step true !debug !quiet [] [] prog
    done

let _ =
  Arg.parse argspec setfile
    "Syntax: arith -e program [-debug] [-show-unimportant]\n";
  go ()
