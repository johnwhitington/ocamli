let _ = Pptinyocaml.simple := true; Ocamliutil.typecheck := false 
let exception_from_ocaml e = Tinyocaml.Unit 
let trip x =
  let open Tinyocaml in
    let tiny_x = Tinyexternal.of_ocaml_value [] x {|int|}  in
    let (_,program) = Tinyocamlrw.of_string {|x * 3|}  in
    let env = [EnvBinding (false, (ref [((PatVar x), tiny_x)]))]  in
    let tiny_result = Eval.eval_until_value true false env program  in
    Tinyexternal.to_ocaml_value tiny_result
  
