open Ocamli2type

(* For now, convert to tinyocaml thence to pptinyocaml. Soon, we will need our own prettyprinter, of course *)
let tinyocaml_op_of_finaltype_op = function
  Add -> Tinyocaml.Add
| Sub -> Tinyocaml.Sub
| Mul -> Tinyocaml.Mul
| Div -> Tinyocaml.Div

let rec tinyocaml_of_finaltype_t' typ = function
  Value x -> tinyocaml_of_ocaml_heap_value typ x
| Var x -> Tinyocaml.Var x
| ArrayExpr arr -> Tinyocaml.Array (Array.map tinyocaml_of_finaltype arr)
| Cons (h, t) -> Tinyocaml.Cons (tinyocaml_of_finaltype h, tinyocaml_of_finaltype t)
| Append (a, b) -> Tinyocaml.Append (tinyocaml_of_finaltype a, tinyocaml_of_finaltype b)
| IntOp (op, x, y) ->
    Tinyocaml.Op
      (tinyocaml_op_of_finaltype_op op,
       tinyocaml_of_finaltype x,
       tinyocaml_of_finaltype y)
| FOp (op, x, y) ->
    Tinyocaml.App
      ((Tinyocaml.App (Var "Stdlib.+.", tinyocaml_of_finaltype x)), tinyocaml_of_finaltype y)
| ArrayGet (x, y) ->
    Tinyocaml.App
      ((Tinyocaml.App
        (Var "Stdlib.Array.get", tinyocaml_of_finaltype x)),
      (tinyocaml_of_finaltype y))
| ArraySet (arr, index, newval) ->
    Tinyocaml.App
      (Tinyocaml.App
        ((Tinyocaml.App
          (Var "Stdlib.Array.set", tinyocaml_of_finaltype arr)),
        (tinyocaml_of_finaltype index)),
        (tinyocaml_of_finaltype newval))
| Let ((n, a), b) ->
    Tinyocaml.Let
      (false,
       [(Tinyocaml.PatVar n, tinyocaml_of_finaltype a)],
       tinyocaml_of_finaltype b)

and tinyocaml_of_finaltype {e; typ; lets} =
  (* If any implicit lets, fabricate them -- but only if they are used in the
   * expression underneath, and not shadowed. *)
  (*Printf.printf "We have %i lets\n" (List.length lets);*)
  let remove_names_from_lets names =
    List.filter (fun (v, _) -> List.mem v names) in
  let rec remove_shadowed_implicits = function
    [] -> []
  | (n, e)::r ->
      if List.mem n (List.map fst r)
        then remove_shadowed_implicits r
        else (n, e)::remove_shadowed_implicits r
  in
  let rec fabricate_lets e = function
    [] -> e
  | (n, rhs)::r ->
      fabricate_lets (Tinyocaml.Let (false, [(Tinyocaml.PatVar n, tinyocaml_of_finaltype rhs)], e)) r
  in
  let inner = tinyocaml_of_finaltype_t' typ e in
    if lets = [] then inner else
      let names = names_in_t' e in
      (*Printf.printf "%i names in t'\n" (List.length names);*)
      let lets_to_print =
        remove_names_from_lets names (remove_shadowed_implicits lets)
      in
        (*Printf.printf "lets to print: %i\n" (List.length lets_to_print);*)
        fabricate_lets inner (List.rev lets_to_print)

