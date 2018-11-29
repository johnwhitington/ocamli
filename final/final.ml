open Finaltype
open Typedtree
open Types

let showfinaltype = ref false
let showsteps = ref false

let is_value_t' = function
  Value _ -> true
| ArrayExpr _ | IntOp _ | FOp _ | ArrayGet _ | ArraySet _ -> false

let is_value {e} = is_value_t' e

let string_of_op = function
    Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div" 

let op_of_text = function
  "+" | "+." -> Add
| "-" | "-." -> Sub
| "*" | "*." -> Mul
| "/" | "/." -> Div
| _ -> failwith "op_of_text"

let rec find_type_desc {desc} =
  match desc with
    Tlink x -> find_type_desc x
  | typ -> typ

let rec to_ocaml_heap_value = function
  Value x -> x
| ArrayExpr arr ->
    (* This arrayexpr contains only values. Turn it into a value itself. *)
    let x = Obj.new_block 0 (Array.length arr) in
      for i = 0 to Array.length arr - 1 do
        Obj.set_field x i (to_ocaml_heap_value arr.(i).e)
      done;
      x
| _ -> failwith "to_ocaml_heap_value: unknown"

let string_of_ocaml_type = function
  Tvar (Some x) -> x
| Tvar None -> "_"
| Tarrow (_, _, _, _) -> "Tarrow"
| Ttuple _ -> "Ttuple"
| Tconstr (path, _, _) -> "Tconstr " ^ Path.name path
| Tobject (_, _) -> "Tobject"
| Tfield (_, _, _, _) -> "Tfield"
| Tnil -> "Tnil"
| Tlink _ -> "Tlink"
| Tsubst _ -> "Tsubst"
| Tvariant _ -> "Tvariant"
| Tunivar _ -> "Tunivar"
| Tpoly (_, _) -> "Tpoly"
| Tpackage (_, _, _) -> "Tpackage"

let rec tinyocaml_of_ocaml_heap_value (typ : type_desc) (value : Obj.t) =
  Printf.printf "tinyocaml_of_ocaml_heap_value: %s\n" (string_of_ocaml_type typ);
  match typ with
    Tconstr (p, _, _) when Path.name p = "int" -> Tinyocaml.Int (Obj.magic value : int)
  | Tconstr (p, _, _) when Path.name p = "float" -> Tinyocaml.Float (Obj.magic value : float)
  | Tconstr (p, _, _) when Path.name p = "unit" -> Tinyocaml.Unit
  | Tconstr (p, [elt_t], _) when Path.name p = "array" ->
      Tinyocaml.Array
        (Array.init
          (Obj.size value)
          (fun i -> tinyocaml_of_ocaml_heap_value (find_type_desc elt_t) (Obj.field value i)))
  | _ -> failwith "tinyocaml_of_ocaml_heap_value: unknown type"

(* For now, convert to tinyocaml thence to pptinyocaml. Soon, we will need our own prettyprinter, of course *)
let tinyocaml_op_of_finaltype_op = function
  Add -> Tinyocaml.Add
| Sub -> Tinyocaml.Sub
| Mul -> Tinyocaml.Mul
| Div -> Tinyocaml.Div

let rec tinyocaml_of_finaltype typ = function
  Value x -> tinyocaml_of_ocaml_heap_value typ x
| ArrayExpr arr -> Tinyocaml.Array (Array.map (fun {typ; e} -> tinyocaml_of_finaltype typ e) arr)
| IntOp (op, {typ = typx; e = ex}, {typ = typy; e = ey}) ->
    Tinyocaml.Op (tinyocaml_op_of_finaltype_op op, tinyocaml_of_finaltype typx ex, tinyocaml_of_finaltype typy ey)
| FOp (op, {typ = typx; e = ex}, {typ = typy; e = ey}) ->
    Tinyocaml.App ((Tinyocaml.App (Var "Stdlib.+.", tinyocaml_of_finaltype typx ex)), (tinyocaml_of_finaltype typy ey))
| ArrayGet ({typ = typx; e = ex}, {typ = typy; e = ey}) ->
    Tinyocaml.App
      ((Tinyocaml.App
        (Var "Stdlib.Array.get", tinyocaml_of_finaltype typx ex)),
      (tinyocaml_of_finaltype typy ey))
| ArraySet ({typ = typ_arr; e = e_arr},
            {typ = typ_index; e = e_index},
            {typ = typ_newval; e = e_newval}) ->
    Tinyocaml.App
      (Tinyocaml.App
        ((Tinyocaml.App
          (Var "Stdlib.Array.set", tinyocaml_of_finaltype typ_arr e_arr)),
        (tinyocaml_of_finaltype typ_index e_index)),
        (tinyocaml_of_finaltype typ_newval e_newval))

let string_of_tinyocaml = Pptinyocaml.to_string

let rec string_of_finaltype_t' typ = function
  Value x -> string_of_tinyocaml (tinyocaml_of_ocaml_heap_value typ x)
| ArrayExpr items ->
    Printf.sprintf "[|%s|]" (string_of_items (Array.to_list items)) 
| FOp (op, a, b) ->
    Printf.sprintf
      "FOp (%s, %s, %s)"
      (string_of_op op) (string_of_finaltype a) (string_of_finaltype b)
| IntOp (op, a, b) ->
    Printf.sprintf
      "IntOp (%s, %s, %s)"
      (string_of_op op) (string_of_finaltype a) (string_of_finaltype b)
| ArrayGet (arr, i) ->
    Printf.sprintf "ArrayGet (%s, %s)" (string_of_finaltype arr) (string_of_finaltype i)
| ArraySet (arr, i, newval) ->
    Printf.sprintf
      "ArraySet (%s, %s, %s)"
      (string_of_finaltype arr) (string_of_finaltype i) (string_of_finaltype newval)

and string_of_items items =
  List.fold_left ( ^ ) "" (List.map string_of_finaltype items)

and string_of_finaltype {typ; e} =
  string_of_finaltype_t' typ e

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

(* If an ArrayExpr contains only things which are values, we need to identify
 * it and turn it into a heap object. However, it cannot be considered really a
 * "value", or it would never get evaluted. E.g [|1 + 2; 3|] is an ArrayExpr,
 * but not (yet) a value *)
let rec array_expr_should_be_value arr =
  Array.for_all
    (function {e = ArrayExpr a} -> array_expr_should_be_value a
            | x -> is_value x)
    arr

let rec eval expr =
  match expr.e with
  FOp (op, {e = Value x}, {e = Value y}) ->
    {expr with e = Value (Obj.repr (perform_float_op op (Obj.magic x : float) (Obj.magic y : float)))}
| FOp (op, ({e = Value _} as x), y) ->
    {expr with e = FOp (op, x, eval y)}
| FOp (op, x, y) ->
    {expr with e = FOp (op, eval x, y)}
| IntOp (op, {e = Value x}, {e = Value y}) ->
    {expr with e = Value (Obj.repr (perform_int_op op (Obj.magic x : int) (Obj.magic y : int)))}
| IntOp (op, ({e = Value _} as x), y) ->
    {expr with e = IntOp (op, x, eval y)}
| IntOp (op, x, y) ->
    {expr with e = IntOp (op, eval x, y)}
| ArrayExpr a ->
    if array_expr_should_be_value a then
      {expr with e = Value (to_ocaml_heap_value (ArrayExpr a))}
    else 
      begin
        if eval_first_non_value_element a
          then {expr with e = ArrayExpr a}
          else assert false
      end
| ArrayGet (arr, i) ->
    if is_value arr then
      match arr, i with
        {e = Value array_val}, {e = Value index} ->
          {expr with e = Value ((Obj.magic array_val : 'a array).((Obj.magic index : int)))}
      | _ -> {expr with e = ArrayGet (arr, eval i)}
    else
      {expr with e = ArrayGet (eval arr, i)}
| ArraySet (arr, i, e) ->
    if not (is_value arr) then {expr with e = ArraySet (eval arr, i, e)}
    else if not (is_value i) then {expr with e = ArraySet (arr, eval i, e)}
    else if not (is_value e) then {expr with e = ArraySet (arr, i, eval e)}
    else
      begin match arr, i, e with
      | {e = Value array_val}, {e = Value i}, {e = Value newval} ->
          (Obj.magic array_val : 'a array).((Obj.magic i : int)) <- newval;
          {expr with e = Value (Obj.repr ())}
      | _ -> assert false
      end
| Value _ -> failwith "already a value"

and eval_first_non_value_element arr =
  try
    for x = 0 to Array.length arr - 1 do
      match arr.(x) with
        {e = ArrayExpr arr'} ->
          if eval_first_non_value_element arr' then raise Exit
      | elt ->
          if not (is_value elt) then (arr.(x) <- eval elt; raise Exit)
    done;
    false
  with
    Exit -> true

let rec eval_full v =
  if !showsteps then Printf.printf "%s\n" (string_of_finaltype v);
  Printf.printf "%s\n" (string_of_tinyocaml (tinyocaml_of_finaltype v.typ v.e));
  if is_value v then v else eval_full (eval v)

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s


let rec finaltype_of_expression_desc = function
  Texp_constant (Const_int x) -> Value (Obj.repr x)
| Texp_constant (Const_float x) -> Value (Obj.repr (float_of_string x))
| Texp_apply
    ({exp_desc =
        Texp_ident (Path.Pdot (Path.Pident i, (("+" | "-" | "*" | "/") as optext)), _, _)},
     [(_, Some arg1); (_, Some arg2)]) when Ident.name i = "Stdlib" ->
       IntOp (op_of_text optext, finaltype_of_expression arg1, finaltype_of_expression arg2)
| Texp_apply
    ({exp_desc =
        Texp_ident (Path.Pdot (Path.Pident i, (("+." | "-." | "*." | "/.") as optext)), _, _)},
     [(_, Some arg1); (_, Some arg2)]) when Ident.name i = "Stdlib" ->
       FOp (op_of_text optext, finaltype_of_expression arg1, finaltype_of_expression arg2)
| Texp_apply
    ({exp_desc =
      Texp_ident (Path.Pdot (Path.Pdot (Path.Pident x, y), z), _, _)},
        [(_, Some arr); (_, Some index)])
      when Ident.name x = "Stdlib" && y = "Array" && z = "get" ->
        ArrayGet (finaltype_of_expression arr, finaltype_of_expression index)
| Texp_apply
    ({exp_desc =
      Texp_ident (Path.Pdot (Path.Pdot (Path.Pident x, y), z), _, _)},
        [(_, Some arr); (_, Some index); (_, Some newval)])
      when Ident.name x = "Stdlib" && y = "Array" && z = "set" ->
        ArraySet
          (finaltype_of_expression arr,
           finaltype_of_expression index,
           finaltype_of_expression newval)
| Texp_array es ->
    let arr = Array.of_list (List.map finaltype_of_expression es) in
      if array_expr_should_be_value arr then
        Value (to_ocaml_heap_value (ArrayExpr arr))
      else
        ArrayExpr arr
| _ -> failwith "finaltype_of_expression_desc: unknown"

and finaltype_of_expression exp =
  {e = finaltype_of_expression_desc exp.exp_desc;
   typ = find_type_desc exp.exp_type}

(* For now just first structure item. To remove later when we have real structure item support. *)
let finaltype_of_typedtree {str_items} =
  match (List.hd str_items).str_desc with
    Tstr_value (_, [vb]) -> finaltype_of_expression vb.vb_expr
  | Tstr_eval (e, _) -> finaltype_of_expression e
  | _ -> failwith "finaltype_of_typedtree"

let env =
  Compmisc.init_path false;
  Compmisc.initial_env ()

let typedtree_of_string ?(filename="") code =
  let ast =
    let lexer = Lexing.from_string code in
    Location.init lexer filename;
    Parse.implementation lexer
  in
    try
      let typedtree, _ = Typemod.type_implementation "foo.ml" "" "example" env ast in
      typedtree
    with
      e ->
        Location.report_exception Format.std_formatter e;
        exit 2

(* Arg. -e or filename. -dfinal to debug initial program as final, and also each step. *)
let programtext = ref ""

let setfile filename =
  programtext := load_file filename

let argspec =
  ["-e", Arg.Set_string programtext, " Set program text";
   "-dfinaltype", Arg.Set showfinaltype, " Show the finaltype representation of the input program";
   "-dsteps", Arg.Set showsteps, " Show information for each step of evaluation"]

let _ =
  Arg.parse argspec setfile "Syntax: final <filename | -e program>\n";
  eval_full (finaltype_of_typedtree (typedtree_of_string !programtext))
