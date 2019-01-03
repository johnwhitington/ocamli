open Ocamli2type

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

(* Pattern matching. FIXME guard etc. Once we get rid of tinyocaml this gets simpler, of course. *)
let rec patmatch expr (pat, guard, rhs) =
  let yes = Some rhs and no = None in
  match expr, pat with
    _, PatAny -> yes
  | {e = Value v}, PatConstant (IntConstant i) ->
      begin match tinyocaml_of_ocaml_heap_value expr.typ v with
        Tinyocaml.Int i' when i = i' -> yes
      | _ -> no
      end
  | {e = Value v}, PatConstr ("[]", _) ->
      begin match tinyocaml_of_ocaml_heap_value expr.typ v with
        Tinyocaml.Nil -> yes
      | _ -> no
      end
  | {e = Value v}, PatConstr ("::", [hpat; tpat]) ->
      (* Make sure value is a cons cell, then try to pattern-match on its head and tail. *)
      (* FIXME: Implicit lets added during hpat and tpat must be amalgamated here? Names cannot clash *)
      (* Once we remove tinyocaml requirement, do we need the types at all? Surely we are in typeless territory here? *)
      begin match tinyocaml_of_ocaml_heap_value expr.typ v with
        Tinyocaml.Cons (a, b) ->
          let htyp =
            match expr.typ with
              Tconstr (_, [elt_t], _) -> find_type_desc elt_t
            | _ -> failwith "patmatch bad list"
          in
          let hval, tval =
            {e = Value (Obj.field v 0); lets = []; typ = htyp},
            {e = Value (Obj.field v 1); lets = []; typ = expr.typ} (* tail has same type *)
          in
            begin match patmatch hval (hpat, None, rhs), patmatch tval (tpat, None, rhs) with
              Some lunder, Some runder ->
                Some {rhs with lets = lunder.lets @ runder.lets @ rhs.lets}
            | _ -> no
            end
      | _ -> no
      end
  | {e = Value v}, PatVar varname ->
      (* Introduce an implicit let into the rhs *)
      Printf.printf "Adding %s to implicit lets of rhs\n" varname;
      Some {rhs with lets = (false, ref [(varname, expr)]) :: rhs.lets}
  | _, _ -> no

(* Look up something in the environment *)
let rec lookup x = function
  [] -> raise Not_found
| (_, {contents = []})::r -> lookup x r
| (recflag, {contents = (n, e)::t})::r ->
    if x = n then e else lookup x ((recflag, {contents = t})::r)

let rec eval env expr =
  let env = List.rev expr.lets @ env in
  match expr.e with
  FOp (op, {e = Value x}, {e = Value y}) ->
    {expr with e = Value (Obj.repr (perform_float_op op (Obj.magic x : float) (Obj.magic y : float)))}
| FOp (op, ({e = Value _} as x), y) ->
    {expr with e = FOp (op, x, eval env y)}
| FOp (op, x, y) ->
    {expr with e = FOp (op, eval env x, y)}
| IntOp (op, {e = Value x}, {e = Value y}) ->
    {expr with e = Value (Obj.repr (perform_int_op op (Obj.magic x : int) (Obj.magic y : int)))}
| IntOp (op, ({e = Value _} as x), y) ->
    {expr with e = IntOp (op, x, eval env y)}
| IntOp (op, x, y) ->
    {expr with e = IntOp (op, eval env x, y)}
| Var x ->
    Printf.printf "looking for var %s in environment of length %i\n" x (List.length env);
    lookup x env
| Let (recflag, (n, exp), exp') ->
    let evalled = eval env exp in
      if is_value evalled
        then
          {e = exp'.e;
           typ = exp.typ;
           lets = expr.lets @ [(recflag, ref [(n, evalled)])] @ exp'.lets}
        else {expr with e = Let (recflag, (n, evalled), exp')}
| ArrayExpr a ->
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
    let evalled =
      if is_value h then Cons (h, eval env t) else Cons (eval env h, t)
    in
      (*Printf.printf "Evalled is %s\n" (string_of_t' expr.typ evalled);*)
      if should_be_value_t' evalled
        then {expr with e = Value (Ocamli2read.to_ocaml_heap_value evalled)}
        else {expr with e = evalled}
| Append (a, b) when is_value a && is_value b ->
    {expr with e = append_lists a.e b.e}
| Append (a, b) ->
    {expr with e =
      if is_value a then Append (a, eval env b) else Append (eval env a, b)}
| ArrayGet (arr, i) ->
    if is_value arr then
      match arr, i with
        {e = Value array_val}, {e = Value index} ->
          {expr with e = Value ((Obj.magic array_val : 'a array).((Obj.magic index : int)))}
      | _ -> {expr with e = ArrayGet (arr, eval env i)}
    else
      {expr with e = ArrayGet (eval env arr, i)}
| ArraySet (arr, i, e) ->
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
    if is_value e then
      begin match patmatch e h with
        Some rhs -> {expr with e = rhs.e; lets = rhs.lets @ expr.lets}
      | None -> {expr with e = Match (e, t)}
      end
    else
      {expr with e = Match (eval env e, h::t)}
| Match (_, []) ->
    failwith "Matched no pattern"
(* 1. Function not a yet a value. Eval one step. *)
| Apply (f, args) when not (is_value f) -> {expr with e = Apply (eval env f, args)}
(* 2. Main case. We have a function and one or more arguments *)
| Apply ({e = Function ((pat, guard, rhs) as p::ps, fenv)} as f, a::ags) ->
    (* See if the case matches, if not move on *)
    begin match patmatch a p with
      None -> {expr with e = Apply ({f with e = Function (ps, fenv)}, [a])}
    | Some rhs ->
        (* We have matched. And so, we see if ags is empty. If it is, we just
         * return the right hand side. If not, we return Apply(rhs, ags) *)
        let rhs' =
          (* When it does, add the bindings from the closure as implicit lets in the rhs *)
          {rhs with lets = fenv @ rhs.lets}
        in
        begin match ags with
          [] -> rhs'
        | ags -> {expr with e = Apply (rhs', ags)} 
        end
    end
| Apply (_, []) -> failwith "empty cases"
| Apply (_, _) -> failwith "malformed Apply on evaluation"
| LetDef (recflag, (n, e)) ->
    {expr with e = LetDef (recflag, (n, eval env e))}
| Struct lst ->
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
