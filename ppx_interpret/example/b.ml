(* This the module we wish to interpret *)

(* Need to (a) Convert x to a heap value from a Tinyocaml.t
 *         (b) Call A.double on it
 *         (c) Convert the result back from a heap value to a Tinyocaml.t *)

(* Original to-be-compiled source: *)
let quad x = A.double x * 2

(* What ppx_interpret will create *)
let a_dot_double_builtin (x : Tinyocaml.t) =
  let open Tinyocaml in
    let f _ xs =
      let heap_xs = List.map (Tinyexternal.to_ocaml_value) xs in
      let result = A.double (List.hd heap_xs) in
      Tinyexternal.of_ocaml_value [] result "int"
    in
      Fun (NoLabel, PatVar "x", CallBuiltIn (None, "A.double", [Var "x"], f), [])

let quad x =
  let open Tinyocaml in
    let tiny_x = Tinyexternal.of_ocaml_value [] x "int" in
    let _, program = Tinyocamlrw.of_string "A.double x * 2" in
    let call_a_dot_double =
      Function ([(PatVar "x", None, a_dot_double_builtin tiny_x)], [])
    in
    let environment =
      [EnvBinding (false, ref [(PatVar "A.double", call_a_dot_double)]);
       EnvBinding (false, ref [(PatVar "x", tiny_x)])]
    in
      let tiny_result = Eval.eval_until_value true true environment program in
      Tinyexternal.to_ocaml_value tiny_result

