open Ocamli2type

let showrules = ref false

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
        let h = {e = Value (Obj.field v 0); lets = expr.lets; typ = htyp}
        and t = {e = Value (Obj.field v 1); lets = expr.lets; typ = expr.typ} in
          begin match patmatch h (hpat, None, rhs), patmatch t (tpat, None, rhs) with
            Some lunder, Some runder ->
              Some {rhs with lets = lunder.lets @ runder.lets @ rhs.lets}
          | _ -> no
          end
  | {e = Value v}, PatVar varname ->
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

let rec eval env expr =
  if !showrules then print_string "RULE: ";
  let env = List.rev expr.lets @ env in
  match expr.e with
  FOp (op, {e = Value x}, {e = Value y}) ->
    if !showrules then print_endline "Float-values";
    {expr with e = Value (Obj.repr (perform_float_op op (Obj.magic x : float) (Obj.magic y : float)))}
| FOp (op, ({e = Value _} as x), y) ->
    if !showrules then print_endline "Float-right";
    {expr with e = FOp (op, x, eval env y)}
| FOp (op, x, y) ->
    if !showrules then print_endline "Float";
    {expr with e = FOp (op, eval env x, y)}
| IntOp (op, {e = Value x}, {e = Value y}) ->
    if !showrules then print_endline "Int-values";
    {expr with e = Value (Obj.repr (perform_int_op op (Obj.magic x : int) (Obj.magic y : int)))}
| IntOp (op, ({e = Value _} as x), y) ->
    if !showrules then print_endline "Int-value-right";
    {expr with e = IntOp (op, x, eval env y)}
| IntOp (op, x, y) ->
    if !showrules then print_endline "Int-value-left";
    {expr with e = IntOp (op, eval env x, y)}
| Var x ->
    if !showrules then print_endline "Var";
    if !showrules then Printf.printf "looking for var %s in environment of length %i\n" x (List.length env);
    lookup x env
| Let (recflag, (n, exp), exp') ->
    if !showrules then print_endline "Let";
    let evalled = eval env exp in
      if is_value evalled
        then
          {e = exp'.e;
           typ = exp.typ;
           lets = expr.lets @ [(recflag, ref [(n, evalled)])] @ exp'.lets}
        else {expr with e = Let (recflag, (n, evalled), exp')}
| ArrayExpr a ->
    if !showrules then print_endline "ArrayExpr";
    let evalled =
      if eval_first_non_value_element env a
        then {expr with e = ArrayExpr a}
        else assert false
    in
      if should_be_value evalled then
        {expr with e = Value (Ocamli2read.to_ocaml_heap_value evalled.e)}
      else
        evalled
| Cons (h, t) ->
    if !showrules then print_endline "Cons";
    let evalled =
      if is_value h then Cons (h, eval env t) else Cons (eval env h, t)
    in
      (*Printf.printf "Evalled is %s\n" (string_of_t' expr.typ evalled);*)
      if should_be_value_t' evalled
        then {expr with e = Value (Ocamli2read.to_ocaml_heap_value evalled)}
        else {expr with e = evalled}
| Append (a, b) when is_value a && is_value b ->
    if !showrules then print_endline "Append-values";
    {expr with e = append_lists a.e b.e}
| Append (a, b) ->
    if !showrules then print_endline "Append";
    {expr with e =
      if is_value a then Append (a, eval env b) else Append (eval env a, b)}
| ArrayGet (arr, i) ->
    if !showrules then print_endline "ArrayGet";
    if is_value arr then
      match arr, i with
        {e = Value array_val}, {e = Value index} ->
          {expr with e = Value ((Obj.magic array_val : 'a array).((Obj.magic index : int)))}
      | _ -> {expr with e = ArrayGet (arr, eval env i)}
    else
      {expr with e = ArrayGet (eval env arr, i)}
| ArraySet (arr, i, e) ->
    if !showrules then print_endline "ArraySet";
    if not (is_value arr) then {expr with e = ArraySet (eval env arr, i, e)}
    else if not (is_value i) then {expr with e = ArraySet (arr, eval env i, e)}
    else if not (is_value e) then {expr with e = ArraySet (arr, i, eval env e)}
    else
      begin match arr, i, e with
      | {e = Value array_val}, {e = Value i}, {e = Value newval} ->
          (Obj.magic array_val : 'a array).((Obj.magic i : int)) <- newval;
          {expr with e = Value (Obj.repr ())}
      | _ -> assert false
      end
| Match (e, h::t) ->
    if !showrules then print_endline "Match";
    if is_value e then
      begin match patmatch e h with
        Some rhs -> {expr with e = rhs.e; lets = rhs.lets @ expr.lets}
      | None -> {expr with e = Match (e, t)}
      end
    else
      {expr with e = Match (eval env e, h::t)}
| Match (_, []) ->
    if !showrules then print_endline "Match";
    failwith "Matched no pattern"
(* 1. Function not a yet a value. Eval one step. *)
| Apply (f, args) when not (is_value f) ->
    if !showrules then print_endline "Apply-fun-not-value";
    {expr with e = Apply (eval env f, args)}
(* 2. First argument not yet a value. *) 
| Apply (f, h::t) when not (is_value h) ->
    if !showrules then print_endline "Apply-arg-not-value";
    {expr with e = Apply (f, eval env h::t)}
(* 3. We have a function and at least one argument which is a value. *)
| Apply ({e = Function ((pat, guard, rhs) as p::ps, fenv); lets = flets} as f, a::ags) ->
    if !showrules then print_endline "Apply";
    (* See if the case matches, if not move on *)
    begin match patmatch a p with
      None -> {expr with e = Apply ({f with e = Function (ps, fenv)}, [a])}
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
| Apply (_, []) -> failwith "empty cases"
| Apply (_, _) -> failwith "malformed Apply on evaluation"
| LetDef (recflag, (n, e)) ->
    if !showrules then print_endline "LetDef";
    {expr with e = LetDef (recflag, (n, eval env e))}
| Struct lst ->
    if !showrules then print_endline "Struct";
    {expr with e = Struct (eval_first_non_value_element_of_list env lst)}
| Value _ | Function _ -> failwith "already a value"

and eval_first_non_value_element_of_list env = function
    [] -> []
  | h::t ->
      if is_value h then
        let env' =
          (* Add any name defined by h, if h is a LetDef, to the environment *)
          match h with
            {e = LetDef (recflag, (n, e))} -> (recflag, ref [(n, e)])::env
          | _ -> env
        in
          h::eval_first_non_value_element_of_list env' t
      else
        eval env h::t

and eval_first_non_value_element env arr =
  try
    for x = 0 to Array.length arr - 1 do
      match arr.(x) with
        {e = ArrayExpr arr'} ->
          if eval_first_non_value_element env arr' then raise Exit
      | elt ->
          if not (is_value elt) then (arr.(x) <- eval env elt; raise Exit)
    done;
    false
  with
    Exit -> true