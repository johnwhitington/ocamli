let _ =
  Pptinyocaml.simple := true;
  Ocamliutil.typecheck := false;
  Ocamlilib.load_stdlib := true;
  Ocamlilib.load_library () 
let mk name f =
  let open Tinyocaml in
    Fun
      (NoLabel, (PatVar "*x"), (CallBuiltIn (None, name, [Var "*x"], f)), [])
  
let exception_from_ocaml e = Tinyocaml.Unit 
let a_dot_double_builtin env =
  function
  | x::[] ->
      let heap_x = Tinyexternal.to_ocaml_value x  in
      let result = A.double heap_x  in
      Tinyexternal.of_ocaml_value env result "int"
  | _ -> failwith "a_dot_double_builtin: arity" 
let double x =
  let open Tinyocaml in
    let tiny_x = Tinyexternal.of_ocaml_value [] x {|int|}  in
    let (_,program) =
      Tinyocamlrw.of_string
        {|let rec double x = if x < 100 then double (x * 2) else A.double x in double x|}
       in
    let env =
      [EnvBinding (false, (ref [((PatVar "x"), tiny_x)]))] @
        [EnvBinding
           (false,
             (ref
                [((PatVar "A.double"),
                   (mk "a_dot_double_builtin" a_dot_double_builtin))]))]
       in
    let tiny_result =
      Eval.eval_until_value true false (env @ (!Eval.lib)) program  in
    (Tinyexternal.to_ocaml_value tiny_result : int)
  
