(* This the module we wish to interpret *)

(* Need to (a) Convert x to a heap value from a Tinyocaml.t
 *         (b) Call A.double on it
 *         (c) Convert the result back from a heap value to a Tinyocaml.t *)

(* Original to-be-compiled source: *)

[%interpret]
external c_function : int -> int = "c_function"

(* A simple function *)
let trip x = c_function x * 3 

(* Here, a function which calls something in another module. *)
let double x = A.double x

(* Here, a function which calls something in this module *)
let f x = double x

(*let _ = (Callback.register "trip") trip
let quad x = c_function ((A.double x) * 2)*)

(* What ppx_interpret will create *)
(*

let _ =
  Pptinyocaml.simple := true;
  Ocamliutil.typecheck := false

let exception_from_ocaml e = Unit

(* Shim for calling ocaml function `c_function` *)
[%%auto {|external c_function : int -> int = "c_function"|}]

let c_function_builtin = snd c_function

(* To be called-back from C. So we must interpret, since it happens in this module. *)
let trip x =
  let open Tinyocaml in
    let tiny_x = Tinyexternal.of_ocaml_value [] x "int" in
    let _, program = Tinyocamlrw.of_string "x + 3" in
    let env = [EnvBinding (false, ref [(PatVar "x", tiny_x)])] in
    let tiny_result = Eval.eval_until_value true false env program in
      Tinyexternal.to_ocaml_value tiny_result

(* FIXME. How do we recognise Callback.register and treat specially? Need we at all -- do it inside ocamliprim from interpreted? *)
let _ = Callback.register "trip" trip

(* Shim for calling A.double *)
let mk name f =
  let open Tinyocaml in
    Fun (NoLabel, PatVar "*x", CallBuiltIn (None, name, [Var "*x"], f), [])

let a_dot_double_builtin =
  let f env = function
    | [x] ->
        let heap_x = Tinyexternal.to_ocaml_value x in
        let result = A.double heap_x in
          Tinyexternal.of_ocaml_value env result "int"
    | _ -> failwith "a_dot_double_builtin: arity"
  in
    mk "A.double" f

(* Shim for interpreting quad *)
let quad x =
  let open Tinyocaml in
    let tiny_x = Tinyexternal.of_ocaml_value [] x "int" in
    let _, program = Tinyocamlrw.of_string "c_function (A.double x * 2)" in
    let env =
      [EnvBinding (false, ref [(PatVar "c_function", c_function_builtin)]);
       EnvBinding (false, ref [(PatVar "A.double", a_dot_double_builtin)]);
       EnvBinding (false, ref [(PatVar "x", tiny_x)])]
    in
      let tiny_result = Eval.eval_until_value true false env program in
        Tinyexternal.to_ocaml_value tiny_result*)

