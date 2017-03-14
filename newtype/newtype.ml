type op = Add | Sub | Mul | Div

let string_of_op = function
  Add -> "Add"
| Sub -> "Sub"
| Mul -> "Mul"
| Div -> "Div"

type binding = string * t

and envitem = bool * binding list ref

and environment = envitem list

and annotation = string

and t =
  {t : t';
   lets : (bool * binding list) list;
   annots : annotation list}

and t' =
  Int of int
| Bool of bool
| Var of string
| If of t * t * t
| Op of op * t * t
| Equals of t * t
| Let of bool * binding list * t
| LetDef of bool * binding list
| Apply of t * t
| Function of string * environment * t
| Struct of t list

let mkt x = {t = x; lets = []; annots = []}

let rec is_value_t' = function
| Let (_, bs, a) -> is_value a && List.for_all is_value_binding bs
| LetDef (_, bs) -> List.for_all is_value_binding bs
| Struct xs -> List.for_all is_value xs
| Int _ | Bool _ | Function _ -> true
| _ -> false

and is_value_binding (_, x) = is_value x

and is_value x = is_value_t' x.t

(* Convert from tinyocaml, building our own fenvs *)
let rec of_tinyocaml env = function
  Tinyocaml.Int i -> mkt (Int i)
| Tinyocaml.Bool b -> mkt (Bool b)
| Tinyocaml.Var v -> mkt (Var v)
| Tinyocaml.If (a, b, Some c) ->
    mkt (If (of_tinyocaml env a, of_tinyocaml env b, of_tinyocaml env c))
| Tinyocaml.Op (Tinyocaml.Mul, a, b) ->
    mkt (Op (Mul, of_tinyocaml env a, of_tinyocaml env b))
| Tinyocaml.Op (Tinyocaml.Sub, a, b) ->
    mkt (Op (Sub, of_tinyocaml env a, of_tinyocaml env b))
| Tinyocaml.Op (Tinyocaml.Add, a, b) ->
    mkt (Op (Add, of_tinyocaml env a, of_tinyocaml env b))
| Tinyocaml.Op (Tinyocaml.Div, a, b) ->
    mkt (Op (Div, of_tinyocaml env a, of_tinyocaml env b))
| Tinyocaml.Cmp (Tinyocaml.EQ, a, b) ->
    mkt (Equals (of_tinyocaml env a, of_tinyocaml env b))
| Tinyocaml.Let (recflag, bindings, e) ->
    let theref = ref [] in
    let env' = (recflag, theref)::env in
    theref := List.map (of_tinyocaml_binding env') bindings;
    let of_e = of_tinyocaml env' e in
    (* If all bindings are values, put this in as an implicit let. *)
    if List.for_all is_value_binding !theref then
      {t = of_e.t; lets = (recflag, !theref)::of_e.lets; annots = of_e.annots}
    else
      mkt (Let (recflag, !theref, of_e))
| Tinyocaml.App (a, b) -> mkt (Apply (of_tinyocaml env a, of_tinyocaml env b))
| Tinyocaml.Fun (_, PatVar v, e, fenv) ->
    mkt (Function (v, env, of_tinyocaml env e))
| Tinyocaml.Function ([(Tinyocaml.PatVar v, None, rhs)], fenv) ->
    mkt (Function (v, env, of_tinyocaml env rhs))
| Tinyocaml.Struct (_, es) ->
    mkt (Struct (of_tinyocaml_many env es))
| e -> failwith (Printf.sprintf "of_tinyocaml: unknown structure %s" (Tinyocaml.to_string e))

and of_tinyocaml_many env = function
  [] -> []
| Tinyocaml.LetDef (recflag, bindings)::es ->
    let theref = ref [] in
    let env' = (recflag, theref)::env in
    theref := List.map (of_tinyocaml_binding env') bindings;
    mkt (LetDef (recflag, !theref))::of_tinyocaml_many env' es
| e::es ->
    of_tinyocaml env e::of_tinyocaml_many env es

and of_tinyocaml_binding env = function
    (PatVar v, t) -> (v, of_tinyocaml env t)
  | _ -> failwith "unknown pattern in of_tinyocaml_binding"

let of_tinyocaml x = of_tinyocaml [] x

let rec to_tinyocaml e =
  match e.lets with
    (recflag, bindings)::t ->
      Tinyocaml.Let
        (recflag,
         List.map to_tinyocaml_binding bindings,
         to_tinyocaml {e with lets = t})
  | [] ->
      match e.t with
        Int i -> Tinyocaml.Int i
      | Bool b -> Tinyocaml.Bool b
      | Var v -> Tinyocaml.Var v
      | If (a, b, c) ->
          Tinyocaml.If (to_tinyocaml a, to_tinyocaml b, Some (to_tinyocaml c))
      | Op (Mul, a, b) ->
          Tinyocaml.Op (Tinyocaml.Mul, to_tinyocaml a, to_tinyocaml b)
      | Op (Sub, a, b) ->
          Tinyocaml.Op (Tinyocaml.Sub, to_tinyocaml a, to_tinyocaml b)
      | Op (Add, a, b) ->
          Tinyocaml.Op (Tinyocaml.Add, to_tinyocaml a, to_tinyocaml b)
      | Op (Div, a, b) ->
          Tinyocaml.Op (Tinyocaml.Div, to_tinyocaml a, to_tinyocaml b)
      | Equals (a, b) ->
          Tinyocaml.Cmp (Tinyocaml.EQ, to_tinyocaml a, to_tinyocaml b)
      | Let (recflag, bindings, e) ->
          Tinyocaml.Let (recflag, List.map to_tinyocaml_binding bindings, to_tinyocaml e)
      | LetDef (recflag, bindings) ->
          Tinyocaml.LetDef (recflag, List.map to_tinyocaml_binding bindings)
      | Apply (a, b) ->
          Tinyocaml.App (to_tinyocaml a, to_tinyocaml b)
      | Function (v, fenv, e) ->
          Tinyocaml.Fun
            (Tinyocaml.NoLabel, Tinyocaml.PatVar v, to_tinyocaml e, to_tinyocaml_fenv fenv)
      | Struct es -> Tinyocaml.Struct (true, List.map to_tinyocaml es)

and to_tinyocaml_binding (v, t) =
  (PatVar v, to_tinyocaml t)

and to_tinyocaml_fenv envitems = [] (* Just used for printing. fenv doesn't matter *)

let rec string_of_t' = function
  Int i -> Printf.sprintf "Int %i" i
| Bool b -> Printf.sprintf "Bool %b" b
| Var v -> Printf.sprintf "Var %s" v
| If (a, b, c) ->
    Printf.sprintf "If (%s, %s, %s)" (string_of_t a) (string_of_t b) (string_of_t c)
| Op (op, a, b) -> Printf.sprintf "%s (%s, %s)" (string_of_op op) (string_of_t a) (string_of_t b)
| Equals (a, b) -> Printf.sprintf "Equals (%s, %s)" (string_of_t a) (string_of_t b)
| Let (recflag, bindings, e) ->
    Printf.sprintf "Let %b, [%s], %s" recflag (string_of_bindings bindings) (string_of_t e) 
| LetDef (recflag, bindings) ->
    Printf.sprintf "LetDef %b, [%s]" recflag (string_of_bindings bindings)
| Apply (f, x) ->
    Printf.sprintf "Apply (%s, %s)" (string_of_t f) (string_of_t x)
| Function (v, _, e) ->
    Printf.sprintf "Function (%s, %s)" v (string_of_t e)
| Struct ts ->
    Printf.sprintf "Struct %s"
      (List.fold_left (fun x y -> x ^ "\n" ^ y) "" (List.map string_of_t ts))

and string_of_implicit_lets = function
  [] -> ""
| (recflag, bindings)::more ->
     Printf.sprintf "%b, %s\n" recflag (string_of_bindings bindings) ^
     string_of_implicit_lets more

and string_of_t e =
  string_of_implicit_lets e.lets ^
  "\n" ^
  string_of_t' e.t

and string_of_binding (varname, t) =
  Printf.sprintf "Binding %s" varname

and string_of_bindings bs =
  List.fold_left
    (fun a b -> a ^ "\n" ^ b)
    ""
    (List.map string_of_binding bs)

let print_env_item (n, bs) =
  Printf.printf "%b, %s\n" n (string_of_bindings !bs)

let print_env (env : environment) =
  List.iter print_env_item env

let rec lookup_in_bindings v bs =
  match bs with
    [] -> None
  | (v', e)::_ when v = v' -> Some e
  | _::bs -> lookup_in_bindings v bs

let rec lookup_in_environment v (env : environment) =
  match env with
    [] -> failwith "lookup_in_environment"
  | (_, {contents = bindings})::more ->
    match lookup_in_bindings v bindings with
      None -> lookup_in_environment v more
    | Some x -> x

let envitem_of_bindings recflag bindings =
  (recflag, ref bindings)

let rec add_implicit_lets_to_environment env = function
  [] -> env
| (recflag, bindings)::more ->
    add_implicit_lets_to_environment
      ((envitem_of_bindings recflag bindings)::env)
      more

let real_op = function
  Mul -> ( * )
| Div -> ( / )
| Add -> ( + )
| Sub -> ( - )

(* Eval-in-one-go. Implicit lets not really relevant here, because we just put
them in the env. *)
let rec eval (env : environment) e =
  let env = add_implicit_lets_to_environment env (List.rev e.lets) in
    match e.t with
      Int _ | Bool _ | Function (_, _, _) -> e
    | Var v ->
        begin try eval env (lookup_in_environment v env) with
          _ ->
           Printf.printf "Looking for %s\n" v;
           print_env env;
           raise Exit
        end
    | If ({t = Bool b}, x, y) -> eval env (if b then x else y)
    | If (c, x, y) -> eval env {e with t = If (eval env c, x, y)}
    | Op (op, x, y) ->
        begin match (eval env x).t, (eval env y).t with
          Int x, Int y -> mkt (Int (real_op op x y))
        | _ -> failwith "eval-minus"
        end
    | Equals (x, y) ->
        begin match (eval env x).t, (eval env y).t with
          Int x, Int y -> mkt (Bool (x = y))
        | _ -> failwith "eval-equals"
        end
    | Apply ({t = Function (v, fenv, b)}, y) ->
        let new_envitem = envitem_of_bindings false [(v, eval env y)] in
          eval (new_envitem :: fenv @ env) b
    | Apply (f, y) ->
        eval env {e with t = Apply (eval env f, eval env y)}
    | Let (recflag, bindings, e) ->
        let env' =
           envitem_of_bindings recflag
             (List.map
               (fun (n, be) ->
                  let benv =
                    if recflag
                      then envitem_of_bindings recflag bindings :: env
                      else env
                  in
                    (n, eval benv be))
               bindings)
           :: env
        in
          eval env' e
    | LetDef (recflag, bindings) ->
        let benv =
          if recflag then envitem_of_bindings recflag bindings :: env else env
        in
          {e with t =
            LetDef (recflag, List.map (fun (n, be) -> (n, eval benv be)) bindings)}
    | Struct es ->
        {e with t = Struct (eval_many env es)}

and eval_many env = function
  [] -> []
| [e] -> [eval env e]
| {t = LetDef (recflag, bindings)} as e::es ->
    let benv = envitem_of_bindings recflag bindings :: env in
      eval (if recflag then benv else env) e :: eval_many benv es
| _ -> failwith "malformed struct: first not a letdef"

(* Eval one step, assuming not already a value *)
let rec seval (env : environment) e =
  (*Printf.printf "seval:: %s\n" (string_of_t' e.t);*)
  let env = add_implicit_lets_to_environment env (List.rev e.lets) in
    match e.t with
    | Var v ->
        begin try lookup_in_environment v env with
          _ ->
           Printf.printf "Looking for %s\n" v;
           print_env env;
           raise Exit
        end
    | If ({t = Bool true}, x, _) | If ({t = Bool false}, _, x) ->
        {x with lets = e.lets @ x.lets}
    | If (c, x, y) -> {e with t = If (seval env c, x, y)}
    | Op (op, {t = Int a}, {t = Int b}) -> {e with t = Int (real_op op a b)}
    | Op (op, ({t = Int _} as a), b) -> {e with t = Op (op, a, seval env b)}
    | Op (op, a, b) -> {e with t = Op (op, seval env a, b)}
    | Equals ({t = Int a}, {t = Int b}) -> {e with t = Bool (a = b)}
    | Equals ({t = Bool a}, {t = Bool b}) -> {e with t = Bool (a = b)}
    | Equals ({t = Int _ | Bool _} as a, b) -> {e with t = Equals (a, seval env b)}
    | Equals (a, b) -> {e with t = Equals (seval env a, b)}
    | Let (recflag, bindings, e') ->
        let new_bindings = seval_first_non_value_binding env bindings in
          if List.for_all is_value_binding new_bindings then
            {lets = (recflag, new_bindings)::e.lets @ e'.lets;
             annots = e'.annots;
             t = e'.t}
          else
            {e with t = Let (recflag, new_bindings, e')}
    | Apply ({t = Function (v, fenv, b)} as f, x) ->
        if is_value x then
          let new_envitem = envitem_of_bindings false [(v, x)] in
            seval (new_envitem :: fenv @ env) b
        else
          let new_let = (false, [(v, x)]) in
            {e with t =
              Apply ({f with lets = new_let::e.lets}, seval env x)}
    | Apply (f, x) -> {e with t = Apply (seval env f, x)}
    | Struct items -> {e with t = Struct (seval_first_non_value env items)}
    | LetDef _ -> failwith "LetDef not in a struct"
    | _ ->
        failwith (Printf.sprintf "seval: %s already a value" (string_of_t e))

and seval_first_non_value env = function
  [] -> []
| {t = LetDef (recflag, bindings)} as h::more ->
     let env = add_implicit_lets_to_environment env (List.rev h.lets) in
       if is_value h then
         h::
           seval_first_non_value
             (envitem_of_bindings recflag bindings :: env)
             more
       else
         {h with t =
           LetDef (recflag, seval_first_non_value_binding env bindings)}::more
| h::t when is_value h -> h::seval_first_non_value env t
| h::t -> seval env h::t

and seval_first_non_value_binding env = function
  [] -> []
| (n, e)::t ->
    if is_value e
      then (n, e)::seval_first_non_value_binding env t
      else (n, seval env e)::t

(* string -> ocaml ast -> tinyocaml ast -> newtype ast *)
let of_program_text s =
  of_tinyocaml (Tinyocamlrw.of_real_ocaml [] (Ocamliutil.ast s))

(* newtype ast -> tinyocaml ast -> pptinyocaml -> string *)
let to_program_text x =
  Pptinyocaml.to_string (to_tinyocaml x)

let show p =
  print_string (to_program_text p);
  print_string "\n"

(* Run the program p *)
let run p = eval [] p

let rec run_step p =
  show p;
  if is_value p then () else
    let p' = seval [] p in
      run_step p'

type mode =
  FromFile of string
| FromText of string

let source = ref None

let setfile s =
  source := Some (FromFile s)

let settext s =
  source := Some (FromText s)

let load_code () =
  match !source with
    Some (FromFile s) -> Some (Ocamliutil.load_file s)
  | Some (FromText s) -> Some s
  | None -> None

let step = ref false

let argspec =
  [("-e", Arg.String settext, " Evaluate the program text given");
   ("-step", Arg.Set step, " Evaluate step-by-step")]

let _ =
  Arg.parse argspec setfile "Syntax: newtype <filename | -e program>\n";
  match load_code () with
    Some code ->
      let p = of_program_text code in
        if !step then
          run_step p
        else
          begin
            show p;
            print_string "\n";
            show (run p)
          end
  | None ->
      Printf.eprintf "No source code provided.\n";
      exit 2

