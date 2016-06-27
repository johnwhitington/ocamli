(* Uses the tiny-ocaml simple AST *)
open Tinyocaml
open Evalutils

type t = Tinyocaml.t

let calc = TinyocamlUtils.calc

let comp = TinyocamlUtils.comp

let is_value = TinyocamlUtils.is_value

let debug = ref false

let fastcurry = ref false

let dopeek = ref true

let docollectunusedlets = ref true

let add_prefix x (y, v) =
  (x ^ "." ^ y, v)

let bound_in_bindings bindings =
  List.flatten (List.map bound_in_pattern (List.map fst bindings))

(* True if a variable appears not shadowed. *)
let rec appears var = function
  Var v when v = var -> true
| Op (_, a, b) | And (a, b) | Or (a, b) | Cmp (_, a, b) | App (a, b)
| Seq (a, b) | Cons (a, b) | Append (a, b) -> appears var a || appears var b
| Constr (_, Some x) -> appears var x
| Constr (_, None) -> false
| While (a, b, c, d) ->
    appears var a || appears var b || appears var c || appears var d
| For (v, a, flag, b, c, copy) ->
    appears var a || appears var b || appears var c || appears var copy
| If (a, b, c) -> appears var a || appears var b || appears var c
| Control (_, x) -> appears var x
| Let (recflag, bindings, e') ->
    if recflag then
      (* Inside expression e', or in a rhs, all bindings in the letrec occlude *)
         (appears var e' || (List.exists (appears var) (List.map snd bindings)))
      && not (List.mem var (bound_in_bindings bindings)) 
    else
      (* If appears in rhs of a let or in (e' but not bound by the let) *)
         List.exists (appears var) (List.map snd bindings)
      || (appears var e' && not (List.mem var (bound_in_bindings bindings)))
| LetDef (recflag, bindings) ->
    (* If recursive, the bound names in all patterns occlude *)
    if recflag then
         not (List.mem var (bound_in_bindings bindings))
      && List.exists (fun (_, e) -> appears var e) bindings
    else
      List.exists (fun (_, e) -> appears var e) bindings
| Match (e, patmatch) ->
    appears var e || List.exists (appears_in_case var) patmatch
| Fun (fname, fexp, env) -> not (List.mem var (bound_in_pattern fname)) && appears var fexp
| Function (cases, env) -> List.exists (appears_in_case var) cases
| Record items ->
    List.exists (fun (_, {contents = e}) -> appears var e) items
| Field (e, n) -> appears var e
| SetField (e, n, e') -> appears var e || appears var e'
| TryWith (e, cases) -> appears var e || List.exists (appears_in_case var) cases
| CallBuiltIn (_, args, _) -> List.exists (appears var) args
| Struct (_, ls) -> List.exists (appears var) ls
| Tuple ls -> List.exists (appears var) ls
| Raise (_, Some x) -> appears var x
| Raise (_, None) -> false
| Assert x -> appears var x
| Int _ | Bool _ | Var _ | Float _ | Unit
| OutChannel _ | InChannel _ | String _ | Nil | ExceptionDef _ -> false

(* True if a) appears unoccluded in the 'when' expression b) appears unoccluded
in the rhs *)
and appears_in_case var (pat, guard, rhs) =
  let bound = bound_in_pattern pat in
    let appears_in_guard =
      match guard with
        None -> false
      | Some g -> appears var g
    in
      (appears_in_guard || appears var rhs) && not (List.mem var bound)

let rec collect_unused_lets = function
  Let (recflag, bindings, e) ->
    if
      List.for_all (fun (_, v) -> is_value v) bindings &&
      not
        (let all_names_bound =
          List.flatten (List.map bound_in_pattern (List.map fst bindings))
        in
          List.exists (fun n -> appears n e) all_names_bound)
    then
      collect_unused_lets e
    else
      Let
        (recflag,
         List.map (fun (n, e) -> (n, collect_unused_lets e)) bindings,
         collect_unused_lets e)
| x -> Tinyocaml.recurse collect_unused_lets x

let print_env env =
  List.iter
    (fun (s, _) -> Printf.printf "%s\n" s)
    env

let lookup_value v env =
  (*Printf.printf "looking up %s\n" v;
  print_env env;*)
  try List.assoc v env with
    Not_found ->
      try List.assoc ("Pervasives." ^ v) env with
        Not_found ->
          (* Huge bodge. We will introduce proper environments for separate
           * modules soon *)
          List.assoc ("List." ^ v) env

(* Evaluate one step, assuming not already a value *)
let lookup_int_var env v =
  match lookup_value v env with
    Int i -> i
  | _ -> failwith "comparison not an integer"

(* The last operation to have been done *)
let last = ref []

exception ExceptionRaised of string * Tinyocaml.t option

let rec append_values x y =
  match x with
    Nil -> y
  | Cons (a, b) -> Cons (a, append_values b y)
  | _ -> failwith "bad append"

type patmatchresult =
  Matched of t
| EvaluatedGuardStep of case
| FailedToMatch

let rec matches expr pattern rhs =
  let yes = Some rhs in
  let no = None in
    match expr, pattern with
      _, PatAny -> yes
    | Unit, PatUnit -> yes
    | Int i, PatInt i' when i = i' -> yes
    | Int32 i, PatInt32 i' when i = i' -> yes
    | Int64 i, PatInt64 i' when i = i' -> yes
    | NativeInt i, PatNativeInt i' when i = i' -> yes
    | String s, PatString s' when s = s' -> yes
    | Char c, PatChar c' when c = c' -> yes
    | Char x, PatCharRange (c, c') when x >= c && x <= c' -> yes 
    | e, PatVar v -> Some (Let (false, [(PatVar v, e)], rhs))
    | Nil, PatNil -> yes
    | Cons (h, t), PatCons (ph, pt) ->
        begin match matches h ph rhs with
        | Some rhs' -> matches t pt rhs'
        | None -> no
        end
    | Tuple es, PatTuple ps ->
        match_many_binders es ps rhs
    | e, PatAlias (a, p) ->
        matches e p (Let (false, [(PatVar a, e)], rhs))
    | e, PatOr (a, b) ->
        begin match matches e a rhs with
          Some _ -> yes
        | _ -> matches e b rhs
        end
    | Constr (y, None), PatConstr (x, None) when x = y -> yes
    | Constr (y, Some yp), PatConstr (x, Some xp)
        when x = y -> matches yp xp rhs
    | _ -> no

and match_many_binders es ps rhs =
  match es, ps with
    [], [] -> Some rhs
  | eh::et, ph::pt ->
      begin match matches eh ph rhs with
        None -> None
      | Some rhs' -> match_many_binders et pt rhs'
      end
  | _ -> failwith "match_many_binders"

(* Remove any binding in [bs] which binds a variable found in [pat]. FIXME this
whole thing is wrong. e.g let x, y, z = ... should go to let x, y, _ if we
must remove z. This will all go away when we deal properly with the lets
problem? *)
let filter_bindings (pat : pattern) (bs : binding list) : binding list =
  Evalutils.option_map
    (function
     | (PatVar x, v) -> if List.mem x (bound_in_pattern pat) then None else Some (PatVar x, v)
     | _ -> None)
    bs

let rec read_bindings (bs : binding list) =
  List.flatten
    (List.map
      (function
        | (PatVar v, e) -> [(v, e)]
        | (PatTuple ps, Tuple ts) -> read_bindings (List.combine ps ts)
        | _ -> [])
      bs)

(* Put all the items in fenv as lets around the expression [e] *)
let build_let_from_fenv_item e (recflag, bindings) =
  Let (recflag, bindings, e)

let build_lets_from_fenv (fenv : env) e =
  List.fold_left build_let_from_fenv_item e fenv

let rec eval peek env expr =
  match expr with
| Assert (Bool false) ->
    Raise ("Assert_failure", Some (Tuple [String "//unknown//"; Int 0; Int 0]))
| Assert (Bool true) -> Unit
| Assert e -> Assert (eval peek env e)
| Control (_, x) -> eval peek env x
| Op (op, Int a, Int b) ->
    last := Arith::!last;
    Int (calc op a b)
| Op (op, Int a, b) -> Op (op, Int a, eval peek env b)
| Op (op, a, b) -> Op (op, eval peek env a, b)
| And (Bool false, _) ->
    last := Boolean::!last;
    Bool false
| And (Bool true, Bool b) ->
    last := Boolean::!last;
    Bool b
| And (Bool true, b) -> eval peek env b
| And (a, b) -> And (eval peek env a, b)
| Or (Bool true, _) ->
    last := Boolean::!last;
    Bool true
| Or (Bool false, Bool b) ->
    last := Boolean::!last;
    Bool b
| Or (Bool false, b) -> eval peek env b
| Or (a, b) -> Or (eval peek env a, b)
| Cmp (op, Int a, Int b) ->
    last := Comparison::!last;
    Bool (comp op a b)
| Cmp (op, Var a, Int b) ->
    last := Comparison::!last;
    Bool (comp op (lookup_int_var env a) b)
| Cmp (op, Int a, Var b) ->
    last := Comparison::!last;
    Bool (comp op a (lookup_int_var env b))
| Cmp (op, Var a, Var b) ->
    last := Comparison::!last;
    Bool (comp op (lookup_int_var env a) (lookup_int_var env b))
| Cmp (op, Int a, b) -> Cmp (op, Int a, eval peek env b)
| Cmp (op, a, b) -> Cmp (op, eval peek env a, b)
| If (Bool true, a, _) -> a
| If (Bool false, _, b) -> b
| If (cond, a, b) -> If (eval peek env cond, a, b)
| Let (recflag, bindings, e) ->
    if List.exists (function (PatVar v, e) -> namestarred v | _ -> false) bindings then
      last := InsidePervasive::!last;
    if List.for_all (fun (_, e) -> is_value e) bindings then
      let env' = read_bindings bindings @ env in
        (* If e is a function closure, move this Let inside the function (unless
        it is occluded. FIXME: Mutually-recursive bindings with a name clash may
        break this. See commentary in programs/and.ml *)
        begin match e with
          Fun (fx, fe, fenv) ->
            begin match filter_bindings fx bindings with
              [] -> Fun (fx, fe, fenv)
            | bindings' -> Fun (fx, Let (recflag, bindings', fe), fenv)
            end
        | Function (cases, fenv) ->
            (* Put in the guard of any case where it appears unoccluded by the
             * pattern. Put in the rhs of any case where it appears unoccluded
             * by the pattern *)
             let add_to_case (pat, guard, rhs) =
               (* Filter the bindings to remove anything bound in the pattern *)
               (* If non-empty, add the let-binding to guard and rhs *)
               let bindings' =
                 List.fold_left
                   (fun a b -> filter_bindings (PatVar b) bindings) bindings (bound_in_pattern pat)
               in
               match bindings' with
                 [] -> (pat, guard, rhs)
               | l ->
                   let rhs' =
                     Let (recflag, bindings', rhs)
                   and guard' =
                     match guard with
                     | None -> None
                     | Some g -> Some (Let (recflag, bindings', g))
                   in
                     (pat, guard', rhs')
             in
               Function (List.map add_to_case cases, fenv)
        | _ -> Let (recflag, bindings, eval peek env' e)
        end
    else
      Let (recflag, eval_first_non_value_binding peek false env [] bindings, e)
| LetDef (recflag, bindings) ->
    if List.for_all (fun (_, e) -> is_value e) bindings
      then
        failwith "letdef already a value"
      else
        LetDef (recflag, eval_first_non_value_binding peek recflag env [] bindings)
| App (Fun ((fname, fexp, fenv) as f), x) ->
    if is_value x
      then build_lets_from_fenv fenv (Let (false, [fname, x], fexp))
      else App (Fun f, eval peek env x)
| App (Function ([], fenv), x) ->
    Raise ("Match_failure", Some (Tuple [String "FIXME"; Int 0; Int 0]))
| App (Function ((p::ps), fenv), x) ->
    if is_value x then 
      let p =
        (* add the closure bindings to guard and rhs if not occluded by particular pattern *)
        let (pat, guard, rhs) = p in
          let patbound = bound_in_pattern pat in
            let fenv' =
              Evalutils.option_map
                (fun (recflag, bindings) ->
                   match filter_bindings pat bindings with
                     [] -> None
                   | bs -> Some (recflag, bindings))
                fenv
            in
              (pat,
               begin match guard with None -> None | Some g -> Some (build_lets_from_fenv fenv' g) end,
               build_lets_from_fenv fenv' rhs) 
      in
        begin match eval_case peek env x p with (* 1 *)
        | Matched e -> e
        | EvaluatedGuardStep p' -> App (Function ((p'::ps), fenv), x)
        | FailedToMatch -> App (Function (ps, fenv), x)
        end
    else
      App (Function ((p::ps), fenv), eval peek env x) (* 2 *)
| App (Var v, x) ->
    begin match lookup_value v env with
      Fun (fname, fexp, fenv) ->
        if is_value x then
          (* We must use fenv to build lets here. This will go away when we have
          implicit-lets in the Tinyocaml.t data type *)
          build_lets_from_fenv fenv (Let (false, [fname, x], fexp))
        else
          App (Var v, eval peek env x)
    | Function cases ->
        eval peek env (App (Function cases, x)) (* FIXME this is substitution *)
    | Var v' ->
        App (Var v', x)
    | got ->
        Printf.printf "Malformed app applying %s\n to %s\n - got %s\n"
        v (Tinyocaml.to_string x) (Tinyocaml.to_string got);
        failwith "malformed app"
    end
| App (App _, _) when !fastcurry ->
    (* 3. FIXME: closure-env-fastcurry *)
    eval_curry peek env expr
| App (f, x) -> App (eval peek env f, x)
| Seq (e, e') ->
    if is_value e then e' else Seq (eval peek env e, e')
| While (Bool false, _, _, _) ->
    Unit
| While (Bool true, e', cg, cb) when not (is_value e') ->
    While (Bool true, eval peek env e', cg, cb)
| While (Bool true, e', cg, cb) when is_value e' ->
    While (cg, cb, cg, cb)
| While (e, e', cg, cb) ->
    While (eval peek env e, e', cg, cb)
| For (v, e, ud, e', e'', copy) when not (is_value e) ->
    For (v, eval peek env e, ud, e', e'', copy)
| For (v, e, ud, e', e'', copy) when not (is_value e') ->
    For (v, e, ud, eval peek env e', e'', copy)
| For (_, Int x, UpTo, Int y, _, _) when x > y -> Unit
| For (_, Int x, DownTo, Int y, _, _) when y > x -> Unit
| For (v, Int x, ud, e', e'', copy) when is_value e'' ->
    For (v, Int (x + 1), ud, e', copy, copy)
| For (v, x, ud, e', e'', copy) ->
    For (v, x, ud, e', eval peek ((v, x)::env) e'', copy)
| Record items ->
    eval_first_non_value_record_item peek env items;
    Record items
| Struct (n, ls) ->
    Struct (n, eval_first_non_value_item peek env [] ls)
| Tuple ls ->
    Tuple (eval_first_non_value_item peek env [] ls)
| Field (Record items, n) -> !(List.assoc n items)
| Field (e, n) -> Field (eval peek env e, n)
| SetField (Record items, n, e) ->
    if is_value e
      then (if not peek then (List.assoc n items) := e; Unit)
      else SetField (Record items, n, eval peek env e)
| SetField (e, n, e') ->
    SetField (eval peek env e, n, e')
| Raise (e, payload) ->
    begin match payload with
      Some x when not (is_value x) ->
        Raise (e, Some (eval peek env x))
    | _ ->
      (* FIXME: Need to include info about the last stage here, since it gets elided *)
      raise (ExceptionRaised (e, payload))
    end
| TryWith (e, cases) ->
    if is_value e then e else
      begin try eval peek env e with
        (* FIXME: Must pattern match on x and payload to make sure the exception
         * is only caught if it matches! *)
        ExceptionRaised (x, payload) -> (match List.hd cases with (_, _, x) -> x) (*FIXME FIXME FIXME *)
      end
| CallBuiltIn (name, args, fn) ->
    if List.for_all is_value args
      then if not peek then fn args else Unit
      else CallBuiltIn (name, eval_first_non_value_item peek env [] args, fn)
| Var v ->
    begin try lookup_value v env with
      Not_found -> failwith (Printf.sprintf "Var %s not found" v)
    end
| Cons (x, y) ->
    if is_value x then
      Cons (x, eval peek env y)
    else
      Cons (eval peek env x, y)
| Append (x, y) ->
    if is_value x && is_value y then
      append_values x y
    else if is_value x then Append (x, eval peek env y)
    else Append (eval peek env x, y)
| Match (_, []) ->
    Raise ("Match_failure", Some (Tuple [String "FIXME"; Int 0; Int 0]))
| Match (x, p::ps) ->
    if not (is_value x) then
      Match (eval peek env x, p::ps)
    else
      begin match eval_case peek env x p with
      | Matched e -> e
      | EvaluatedGuardStep p' -> Match (x, p'::ps)
      | FailedToMatch -> Match (x, ps)
      end
| Int _ | Bool _ | Float _ | Fun _ | Unit | OutChannel _
| InChannel _ | String _ | Nil | ExceptionDef _ | TypeDef _ -> failwith "already a value"

and eval_case peek env expr (pattern, guard, rhs) =
  match matches expr pattern rhs with
  | Some rhs' ->
    begin match guard with
      None -> Matched rhs'
    | Some (Bool false) -> FailedToMatch
    | Some (Bool true) -> Matched rhs'
    | Some x -> EvaluatedGuardStep (pattern, Some (eval peek env x), rhs')
    end
  | None -> FailedToMatch

(* Apply curried function appliation App (App (f, x), x') etc. *)
(* 1. If the function 'f' is not a value, evaluate one step *)
(* 2. If any argument is not a value, evaluate one step *)
(* 3. Otherwise, we have a function and some value-arguments, so build the lets *)
and eval_curry_inner peek env e =
  match e with
    App (App _ as f', x') ->
      if is_value x' then
        let f'', did = eval_curry_inner peek env f' in
          (App (f'', x'), did)
      else
        (App (f', eval peek env x'), true)
  | App (f, x) when not (is_value f) -> (App (eval peek env f, x), true)
  | App (f, x) when not (is_value x) -> (App (f, eval peek env x), true)
  | x -> (x, false)

and eval_curry_findfun = function
  App (App (e, _), _) -> eval_curry_findfun e
| App (f, _) -> f
| Fun f -> Fun f
| e -> failwith (Printf.sprintf "eval_curry_findfun: %s" (Tinyocaml.to_string e))

and eval_curry peek env e =
  let x, did = eval_curry_inner peek env e in
    if did then x else
      eval_curry_makelets (eval_curry_findfun e) (eval_curry_collect_args [] e)

and eval_curry_collect_args args = function
    App (f, e) -> eval_curry_collect_args (e::args) f
  | _ -> args

and eval_curry_makelets f args =
  match f, args with
  | Fun (a, fexp, fenv), [x] ->
      Let (false, [a, x], fexp)
  | Fun (a, fexp, fenv), x::xs ->
      Let (false, [a, x], eval_curry_makelets fexp xs)
  | _ -> failwith "eval_curry_makelets"

and eval_first_non_value_item peek env r = function
  [] -> List.rev r
| h::t ->
    let env_entries_of_bindings =
      Evalutils.option_map
        (function (PatVar x, y) -> Some (x, y) | _ -> None)
    in
      if is_value h
        then
          let env' =
            match h with
              LetDef (_, bs) ->  env_entries_of_bindings bs @ env
            | _ -> env
          in
            eval_first_non_value_item peek env' (h::r) t
        else List.rev r @ [eval peek env h] @ t

and eval_first_non_value_binding peek recflag env r = function
  [] -> List.rev r
| (v, e)::t ->
    let env' =
      if recflag then
        match v with PatVar v -> (v, e)::env | _ -> env
      else env
    in
      if is_value e then
          eval_first_non_value_binding peek recflag env' ((v, e)::r) t
      else
        List.rev r @ [(v, eval peek env' e)] @ t

and eval_first_non_value_record_item peek env items =
  try
    List.iter (fun (_, v) -> if not (is_value !v) && not peek then v := eval peek env !v) items
  with
    Exit -> ()

let pervasives =
  List.map (add_prefix "Pervasives") Core.pervasives

let definitions_of_module = function
  Struct (_, items) ->
    Evalutils.option_map
      (fun x ->
        match x with
          LetDef (_, [(PatVar v, def)]) -> Some (v, def)
        | _ -> None) 
      items

let stdlib_dir =
  let tname = Filename.temp_file "ocaml" "ocamli" in
    ignore (Sys.command ("ocamlc -config >" ^ tname));
    let tmp = open_in tname in
      let line = ref "" in
        try
          while true do
            let s = input_line tmp in
              if
                String.length s >= 18 &&
                String.sub s 0 18 = "standard_library: "
              then
                line := s
          done;
          assert false
        with
          End_of_file ->
            close_in tmp;
            Sys.remove tname;
            if !line <> "" then
              (Filename.dir_sep ^
              (String.sub !line 19 (String.length !line - 19)))
            else
              raise (Failure "could not find standard library")

(* Load a module from disk *)
let load_module name file =
  (*Printf.printf "Loading module %s...%!" name;*)
  let themod = Tinyocaml.of_real_ocaml (ast (load_file file)) in
    let themod' = eval false (*FIXME current env! *)pervasives themod in
      (*Printf.printf "done\n%!";*)
      List.rev (List.map (add_prefix name) (definitions_of_module themod'))

let stdlib_list =
  load_module "List" (Filename.concat stdlib_dir "list.ml")

let stdlib_pervasives =
  load_module "Pervasives" (Filename.concat "stdlib" "pervasives.ml")

(*let _ =
  Printf.printf "Got %i definitions from pervasives\n" (List.length
  stdlib_pervasives)*)

let lib =
  stdlib_list @ stdlib_pervasives @ pervasives

(*let _ =
  List.iter
    (fun (n, v) -> Printf.printf "%s = %s\n" n (Pptinyocaml.to_string v)) lib*)

let init x =
  Tinyocaml.of_real_ocaml x

let init_from_tinyocaml x = x

let next e =
  last := [];
  try
    if is_value e
      then IsValue
      else Next ((if !docollectunusedlets then collect_unused_lets else (fun x ->x)) (eval false lib e))
  with
    ExceptionRaised (s, payload) -> raise (ExceptionRaised (s, payload))
  | x ->
      Printf.printf "Error in environment %s\n" (Printexc.to_string x);
      if !debug then raise Exit;
      Malformed "environment"

let to_string x =
  Pptinyocaml.to_string (TinyocamlUtils.underline_redex x) 

let tiny x = TinyocamlUtils.underline_redex x

let peek x =
  if is_value x || not !dopeek then [] else
    let t = !last in
      last := [];
      ignore (eval true lib x);
      let r = !last in
        last := t;
        r

let last x = !last

let newlines = function
  Struct (_, _::_::_) -> true
| _ -> false

