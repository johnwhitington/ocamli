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
external c_function : int -> int = "c_function"
let c_function =
  let f =
    function
    | env ->
        (function
         | (Tinyocaml.Int a)::[] ->
             (try Tinyocaml.Int (c_function a)
              with | e -> exception_from_ocaml e)
         | _ -> failwith "c_function")
     in
  ("c_function",
    (Tinyocaml.Fun
       (Tinyocaml.NoLabel, (Tinyocaml.PatVar "*a"),
         (Tinyocaml.CallBuiltIn (None, "c_function", [Tinyocaml.Var "*a"], f)),
         [])))
  
let c_function_builtin = snd c_function 
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
    let (_,program) = Tinyocamlrw.of_string {|A.double x|}  in
    let env =
      [EnvBinding (false, (ref [((PatVar "x"), tiny_x)]));
      EnvBinding
        (false,
          (ref
             [((PatVar "A.double"),
                (mk "a_dot_double_builtin" a_dot_double_builtin))]));
      EnvBinding (false, (ref [((PatVar "c_function"), c_function_builtin)]))]
       in
    let tiny_result =
      Eval.eval_until_value true false (env @ (!Eval.lib)) program  in
    (Tinyexternal.to_ocaml_value tiny_result : int)
  
let a_dot_double_builtin env =
  function
  | x::[] ->
      let heap_x = Tinyexternal.to_ocaml_value x  in
      let result = A.double heap_x  in
      Tinyexternal.of_ocaml_value env result "int"
  | _ -> failwith "a_dot_double_builtin: arity" 
let trip x =
  let open Tinyocaml in
    let tiny_x = Tinyexternal.of_ocaml_value [] x {|int|}  in
    let (_,program) =
      Tinyocamlrw.of_string
        {|let double x = A.double x in let () = Callback.register "double" double in
    c_function x * 3|}
       in
    let env =
      [EnvBinding (false, (ref [((PatVar "x"), tiny_x)]));
      EnvBinding
        (false,
          (ref
             [((PatVar "A.double"),
                (mk "a_dot_double_builtin" a_dot_double_builtin))]));
      EnvBinding (false, (ref [((PatVar "c_function"), c_function_builtin)]))]
       in
    let tiny_result =
      Eval.eval_until_value true false (env @ (!Eval.lib)) program  in
    (Tinyexternal.to_ocaml_value tiny_result : int)
  
let a_dot_double_builtin env =
  function
  | x::[] ->
      let heap_x = Tinyexternal.to_ocaml_value x  in
      let result = A.double heap_x  in
      Tinyexternal.of_ocaml_value env result "int"
  | _ -> failwith "a_dot_double_builtin: arity" 
let f x =
  let open Tinyocaml in
    let tiny_x = Tinyexternal.of_ocaml_value [] x {|int|}  in
    let (_,program) = Tinyocamlrw.of_string {|double x|}  in
    let env =
      [EnvBinding (false, (ref [((PatVar "x"), tiny_x)]));
      EnvBinding
        (false,
          (ref
             [((PatVar "A.double"),
                (mk "a_dot_double_builtin" a_dot_double_builtin))]));
      EnvBinding (false, (ref [((PatVar "c_function"), c_function_builtin)]))]
       in
    let tiny_result =
      Eval.eval_until_value true false (env @ (!Eval.lib)) program  in
    (Tinyexternal.to_ocaml_value tiny_result : int)
  
