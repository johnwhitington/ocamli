let _ = Pptinyocaml.simple := true; Ocamliutil.typecheck := false 
let mk name f =
  let open Tinyocaml in
    Fun
      (NoLabel, (PatVar "*x"), (CallBuiltIn (None, name, [Var "*x"], f)), [])
  
let exception_from_ocaml e = Tinyocaml.Unit 
let trip x =
  let open Tinyocaml in
    let tiny_x = Tinyexternal.of_ocaml_value [] x {|int|}  in
    let (_,program) = Tinyocamlrw.of_string {|x * 3|}  in
    let env = [EnvBinding (false, (ref [((PatVar "x"), tiny_x)]))]  in
    let tiny_result = Eval.eval_until_value true false env program  in
    (Tinyexternal.to_ocaml_value tiny_result : int)
  
let double x =
  let open Tinyocaml in
    let tiny_x = Tinyexternal.of_ocaml_value [] x {|int|}  in
    let (_,program) =
      Tinyocamlrw.of_string
        {|[1mlet [0ma_dot_double_builtin = [1mlet [0mf env = [1mfunction [0m|
    x::[][1m -> [0m[1mlet [0mheap_x = Tinyexternal.to_ocaml_value x[1m in
    [0m[1mlet [0mresult = A.double heap_x[1m in
    [0mTinyexternal.of_ocaml_value env result "int" | _[1m -> [0mfailwith
    "a_dot_double_builtin: arity" [1m in [0mmk A.double f x|}
       in
    let env = [EnvBinding (false, (ref [((PatVar "x"), tiny_x)]))]  in
    let tiny_result = Eval.eval_until_value true false env program  in
    (Tinyexternal.to_ocaml_value tiny_result : int)
  
