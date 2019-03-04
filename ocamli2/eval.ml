open Type
open Types

let showrules = ref false
let showsteps = ref false
let peek = ref true
let print = ref false
let first = ref true

let indent firstlinearrow str =
  firstlinearrow ^ (Util.string_replace_all "\n" "\n   " str)

let contains_newline s =
  Util.string_replace_all "\n" "xx" s <> s

(* Now, the evaluator *)
let perform_float_op op x y =
  match op with
    Add -> x +. y
  | Sub -> x -. y
  | Mul -> x *. y
  | Div -> x /. y

let perform_int_op op x y =
  match op with
    Add -> x + y
  | Sub -> x - y
  | Mul -> x * y
  | Div -> x / y

(* Append two lists which are 'a values *)
let append_lists a b =
  match a, b with
    Value a, Value b ->
      Value (Obj.magic ((Obj.magic a : 'a list) @ (Obj.magic b : 'a list)) : Obj.t)
  | _ -> assert false

(* One or both lists may be polymorphic. If we can find an element type for one
 * of them, this is our type for the appended list *)
let append_lists_promote_type ta tb =
  match ta with
    Tconstr (_, [{desc = Tvar None}], _) -> tb
  | _ -> ta

(* Pattern matching. *)
let rec patmatch expr (pat, guard, rhs) =
  let yes = Some rhs and no = None in
  match expr, pat with
    _, PatAny -> yes
  | {e = Value v}, PatConstant (IntConstant i) ->
      if v = Obj.repr i then yes else no
  | {e = Value v}, PatConstr ("[]", _) ->
      if v = Obj.repr [] then yes else no
  | {e = Value v}, PatConstr ("::", [hpat; tpat]) ->
      (* FIXME: Implicit lets added during hpat and tpat must be amalgamated here? Names cannot clash *)
      if v = Obj.repr [] then no else
        let htyp =
          match expr.typ with
            Tconstr (_, [elt_t], _) -> find_type_desc elt_t
          | _ -> failwith "patmatch bad list"
        in
        let h = {expr with e = Value (Obj.field v 0); typ = htyp}
        and t = {expr with e = Value (Obj.field v 1); typ = expr.typ} in
          begin match patmatch h (hpat, None, rhs), patmatch t (tpat, None, rhs) with
            Some lunder, Some runder ->
              Some {rhs with lets = lunder.lets @ runder.lets @ rhs.lets}
          | _ -> no
          end
  | _, PatVar varname ->
      (* Introduce an implicit let into the rhs *)
      if !showrules then Printf.printf "Adding %s to implicit lets of rhs\n" varname;
      Some {rhs with lets = (false, ref [(varname, expr)]) :: rhs.lets}
  | _, _ -> no

(* Look up something in the environment *)
let rec lookup x = function
  [] -> raise Not_found
| (_, {contents = []})::r -> lookup x r
| (recflag, {contents = (n, e)::t})::r ->
    if x = n then e else lookup x ((recflag, {contents = t})::r)

let underline expr =
  {expr with peek = Some {underline = true}}

let compare_func_of_op = function
  LT -> ( < )
| GT -> ( > )
| EQ -> ( = )
| GTE -> ( >= )
| LTE -> ( <= )
| NEQ -> ( <> )

(* Take something which should be a value, e.g Cons (Value _, Value _)
 * representing [1] and give it the appropriate type. Due to polymorphism,
 * there may be a TVar None in the type of the whole expression.*)
(* FIXME should use Types.type_expr not Types.type_desc in type.t? *)
let type_ocaml_heap_value = function
  Cons (h, t) ->
    begin match t.typ with
      Tconstr (path, _, abbrev) ->
        Tconstr (path, [{desc = h.typ; level = 0; scope = 0; id = 0}], abbrev) 
    | _ -> failwith "type_ocaml_heap_value2"
    end
| _ -> failwith "type_ocaml_heap_value"

(* Make the native function to call the interpreted function f, so we can pass it to a built-in e.g in the case of
 *
 * List.map (fun x -> x + 1) [1; 2; 3]
 *
 * where List.map is a CallBuiltIn *)
  (* The function, when called with one argument (for now),
   * calls the interpreter repeatedly until there is a value. Then it returns that value *)
let mkempty e =
  {e; lets = []; peek = None; printas = None; typ = Types.Tvar None}

let rec make_native = function
  | Function ([PatVar vname, None, rhs], fenv) as f ->
      let fn =
        let expr =
          mkempty (Apply (mkempty f, [mkempty (Var vname)]))
        in
          fun x ->
            match (eval_full ((false, ref [(vname, mkempty (Value x))])::fenv) expr).e with
              Value ret -> ret
            | _ -> failwith "didn't make a value"
      in
        (Obj.magic fn : Obj.t)
  | _ -> failwith "make_native not a function"

and eval env peek expr =
  if !showrules then print_string "RULE: ";
  let env = List.rev expr.lets @ env in
  match expr.e with
  FOp (op, {e = Value x}, {e = Value y}) ->
    if !showrules then print_endline "Float-values";
    if peek then underline expr else
    {expr with e = Value (Obj.repr (perform_float_op op (Obj.magic x : float) (Obj.magic y : float)))}
| FOp (op, ({e = Value _} as x), y) ->
    if !showrules then print_endline "Float-right";
    {expr with e = FOp (op, x, eval env peek y)}
| FOp (op, x, y) ->
    if !showrules then print_endline "Float";
    {expr with e = FOp (op, eval env peek x, y)}
| IntOp (op, {e = Value x}, {e = Value y}) ->
    if !showrules then print_endline "Int-values";
    if peek then underline expr else
    {expr with e = Value (Obj.repr (perform_int_op op (Obj.magic x : int) (Obj.magic y : int)))}
| IntOp (op, ({e = Value _} as x), y) ->
    if !showrules then print_endline "Int-value-right";
    {expr with e = IntOp (op, x, eval env peek y)}
| IntOp (op, x, y) ->
    if !showrules then print_endline "Int-value-left";
    {expr with e = IntOp (op, eval env peek x, y)}
| Compare (op, {e = Value x}, {e = Value y}) ->
    if !showrules then print_endline "Compare-value-both";
    if peek then underline expr else
    {expr with e = Value (Obj.magic ((compare_func_of_op op) x y) : Obj.t)}
| Compare (op, x, ({e = Value _} as y)) ->
    if !showrules then print_endline "Compare-value-right";
    {expr with e = Compare (op, eval env peek x, y)}
| Compare (op, x, y) ->
    if !showrules then print_endline "Compare";
    {expr with e = Compare (op, x, eval env peek y)}
| BoolOp (op, ({e = Value _} as x), y) ->
    if !showrules then print_endline "Boolop-value-check";
    if peek then underline expr else
    if op = AND && not (Obj.magic x : bool) then {expr with e = Value (Obj.repr false)} else
    if op = OR && (Obj.magic x : bool) then {expr with e = Value (Obj.repr true)} else
    y
| BoolOp (op, x, y) ->
    if !showrules then print_endline "Boolop-left-non-value";
    {expr with e = BoolOp (op, eval env peek x, y)}
| Var x ->
    if !showrules then print_endline "Var";
    if !showrules then Printf.printf "looking for var %s in environment of length %i\n" x (List.length env);
    let value = try lookup x env with Not_found -> failwith ("not in environment: " ^ x) in
      begin match value.e with
        Function _ ->
          if peek then underline expr else {value with printas = match value.printas with None -> Some x | _ -> value.printas}
      | _ ->
          if peek then underline expr else value
      end
| Let (recflag, (n, exp), exp') ->
    if !showrules then print_endline "Let";
    let evalled = eval env peek exp in
      (* If peek is true, must have underlined something, so just return it with the peek-evalled part *)
      if peek then {expr with e = Let (recflag, (n, evalled), exp')} else
        if is_value evalled
          then
            {e = exp'.e;
             typ = exp.typ;
             lets = expr.lets @ [(recflag, ref [(n, evalled)])] @ exp'.lets;
             peek = expr.peek;
             printas = None}
          else {expr with e = Let (recflag, (n, evalled), exp')}
| ArrayExpr a ->
    if !showrules then print_endline "ArrayExpr";
    let evalled =
      if eval_first_non_value_element env peek a
        then {expr with e = ArrayExpr a}
        else assert false
    in
      if peek then evalled else
      if should_be_value evalled then
        {expr with e = Value (Read.to_ocaml_heap_value evalled.e)}
      else
        evalled
| Cons (h, t) ->
    if !showrules then print_endline "Cons";
    let evalled =
      if is_value h then Cons (h, eval env peek t) else Cons (eval env peek h, t)
    in
      if peek then {expr with e = evalled} else
      (*Printf.printf "Evalled is %s\n" (string_of_t' expr.typ evalled);*)
      if should_be_value_t' evalled
        then
          {expr with
             e = Value (Read.to_ocaml_heap_value evalled);
             typ = type_ocaml_heap_value evalled}
        else {expr with e = evalled}
| Append (a, b) when is_value a && is_value b ->
    if !showrules then print_endline "Append-values";
    if peek then underline expr else
    {expr with e = append_lists a.e b.e; typ = append_lists_promote_type a.typ b.typ}
| Append (a, b) ->
    if !showrules then print_endline "Append";
    {expr with e =
      if is_value a then Append (a, eval env peek b) else Append (eval env peek a, b)}
| ArrayGet (arr, i) ->
    if !showrules then print_endline "ArrayGet";
    if is_value arr then
      match arr, i with
        {e = Value array_val}, {e = Value index} ->
          if peek then underline expr else
          {expr with e = Value ((Obj.magic array_val : 'a array).((Obj.magic index : int)))}
      | _ -> {expr with e = ArrayGet (arr, eval env peek i)}
    else
      {expr with e = ArrayGet (eval env peek arr, i)}
| ArraySet (arr, i, e) ->
    if !showrules then print_endline "ArraySet";
    if not (is_value arr) then {expr with e = ArraySet (eval env peek arr, i, e)}
    else if not (is_value i) then {expr with e = ArraySet (arr, eval env peek i, e)}
    else if not (is_value e) then {expr with e = ArraySet (arr, i, eval env peek e)}
    else
      if peek then underline expr else
      begin match arr, i, e with
      | {e = Value array_val}, {e = Value i}, {e = Value newval} ->
          (Obj.magic array_val : 'a array).((Obj.magic i : int)) <- newval;
          {expr with e = Value (Obj.repr ())}
      | _ -> assert false
      end
| Match (e, h::t) ->
    if !showrules then print_endline "Match";
    if is_value e then
      if peek then underline expr else
        begin match patmatch e h with
          Some rhs -> {expr with e = rhs.e; lets = rhs.lets @ expr.lets}
        | None -> {expr with e = Match (e, t)}
        end
    else
      {expr with e = Match (eval env peek e, h::t)}
| Match (_, []) ->
    if !showrules then print_endline "Match";
    if peek then underline expr else
    failwith "Matched no pattern"
(* We have a variable for the function. Look it up and carry straight on to apply it. *)
| Apply ({e = Var v} as f, args) ->
    if peek then underline expr else
      eval env peek {expr with e = Apply (eval env peek f, args)}
(* 1. Function not a yet a value. Eval one step. *)
| Apply (f, args) when not (is_value f) ->
    if !showrules then print_endline "Apply-fun-not-value";
    {expr with e = Apply (eval env peek f, args)}
(* 2. First argument not yet a value. *) 
| Apply (f, h::t) when not (is_value h) ->
    if !showrules then print_endline "Apply-arg-not-value";
    {expr with e = Apply (f, eval env peek h::t)}
(* 3. We have a function and at least one argument which is a value. *)
| Apply ({e = Function ((pat, guard, rhs) as p::ps, fenv); lets = flets} as f, a::ags) ->
    if !showrules then print_endline "Apply";
    (* See if the case matches, if not move on *)
    if peek then underline expr else
    begin match patmatch a p with
      None ->
        begin match ps with
        [] ->
          failwith
            (Printf.sprintf "Pattern-match failed on function, matching pattern %s to value %s"
              (Print.string_of_pattern ((fun (x, _, _) -> x) p)) (Print.to_string a))
        | _ ->
          {expr with e = Apply ({f with e = Function (ps, fenv)}, [a])}
        end
    | Some rhs ->
        (* We have matched. And so, we see if ags is empty. If it is, we just
         * return the right hand side. If not, we return Apply(rhs, ags) *)
        let rhs' =
          (* When it does, add the bindings from the closure as implicit lets in the rhs *)
          {rhs with lets = fenv @ expr.lets @ flets @ rhs.lets}
        in
        begin match ags with
          [] -> rhs'
        | ags -> {expr with e = Apply (rhs', ags)} 
        end
    end
| Apply ({e = Value f}, [{e = Function _ as fi}]) ->
    if !showrules then print_endline "Apply-Builtin-Interp-Final";
    if peek then underline expr else
      {expr with e = Value ((Obj.magic f : Obj.t -> Obj.t) (make_native fi))}
| Apply ({e = Value f} as lhs, {e = Function _ as fi}::more) ->
    if !showrules then print_endline "Apply-BuiltIn-Interp-Partial";
    if peek then underline expr else
      {expr with e = Apply ({lhs with e = Value ((Obj.magic f : Obj.t -> Obj.t) (make_native fi))}, more)}
| Apply ({e = Value f}, [{e = Value v}]) ->
    if !showrules then print_endline "Apply-BuiltIn-Final";
    if peek then underline expr else
      {expr with e = Value ((Obj.magic f : Obj.t -> Obj.t) v)}
| Apply ({e = Value f} as lhs, {e = Value v}::more) ->
    if !showrules then print_endline "Apply-BuiltIn-Partial";
    if peek then underline expr else
      {expr with e = Apply ({lhs with e = Value ((Obj.magic f : Obj.t -> Obj.t) v)}, more)}
| Apply ({e = Function ([], _)}, _) -> failwith "Apply: empty function"
| Apply ({e = Function _}, _) -> failwith "Apply: don't understand this function"
| Apply (_, []) -> failwith "Apply: empty cases"
| Apply (_, _) ->
    failwith (Printf.sprintf "Apply: malformed Apply on evaluation:\n %s\n" (Print.string_of_t expr))
| LetDef (recflag, (n, e)) ->
    if !showrules then print_endline "LetDef";
    let evalled = eval env peek e in
    (* We copy the type of the letdef to the type of the rhs, since it may not
     * have one due to polymorphism *)
    {expr with e = LetDef (recflag, (n, {evalled with typ = expr.typ}))}
| Struct lst ->
    if !showrules then print_endline "Struct";
    {expr with e = Struct (eval_first_non_value_element_of_list env peek lst)}
| Value _ | Function _ -> if peek then expr else failwith "already a value"

and eval_first_non_value_element_of_list env peek = function
    [] -> []
  | h::t ->
      if is_value h then
        let env' =
          (* Add any name defined by h, if h is a LetDef, to the environment *)
          match h with
            {e = LetDef (recflag, (n, e))} -> (recflag, ref [(n, e)])::env
          | _ -> env
        in
          h::eval_first_non_value_element_of_list env' peek t
      else
        eval env peek h::t

and eval_first_non_value_element env peek arr =
  try
    for x = 0 to Array.length arr - 1 do
      match arr.(x) with
        {e = ArrayExpr arr'} ->
          if eval_first_non_value_element env peek arr' then raise Exit
      | elt ->
          if not (is_value elt) then (arr.(x) <- eval env peek elt; raise Exit)
    done;
    false
  with
    Exit -> true

and eval_full env v =
  let pre () = let r = if !first then "   " else "=> " in first := false; r in
  if !showsteps then Printf.printf "%s\n" (Print.string_of_t v);
  if !print then
    begin
      let str = Print.to_string v in
        print_endline (indent (pre ()) str);
        if contains_newline str then print_newline ();
        exit 0
    end
  else
    begin
      flush stdout; if !showrules then print_endline "---Beginning of evaluation";
      let evalled = if !peek then eval env true v else v in
      flush stdout; if !showrules then print_endline "---End of evaluation, beginning of printing";
      let str = Print.to_string evalled in
        print_endline (indent (pre ()) str);
        flush stdout; if !showrules then print_endline "---End of printing";
        if contains_newline str then print_newline ();
        flush stdout; if Type.is_value v then v else eval_full env (eval env false v)
    end
