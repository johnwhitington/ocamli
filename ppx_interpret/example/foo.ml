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
[%%auto {|external c_function : int -> int = "c_function"|}]
let c_function_builtin = snd c_function 
let double x =
  let module A =
    struct
      let double env =
        function
        | x::[] ->
            let heap_x = Tinyexternal.to_ocaml_value x  in
            let result = A.double heap_x  in
            Tinyexternal.of_ocaml_value env result "int"
        | _ -> failwith "A.double: arity" 
    end in
    let open Tinyocaml in
      let tiny_x = Tinyexternal.of_ocaml_value [] x {|int|}  in
      let (_,program) =
        Tinyocamlrw.of_string
          {|let rec double x = if x < 100 then double (x * 2) else A.double x in double x|}
         in
      let env =
        [EnvBinding (false, (ref [((PatVar "x"), tiny_x)]))] @
          ([EnvBinding
              (false,
                (ref [((PatVar "A.double"), (mk "A.double" A.double))]))]
             @
             [EnvBinding
                (false, (ref [((PatVar "c_function"), c_function_builtin)]))])
         in
      let tiny_result =
        Eval.eval_until_value true false (env @ (!Eval.lib)) program  in
      (Tinyexternal.to_ocaml_value tiny_result : int)
  
let trip x =
  let open Tinyocaml in
    let tiny_x = Tinyexternal.of_ocaml_value [] x {|int|}  in
    let (_,program) =
      Tinyocamlrw.of_string
        {|let rec trip x = let double x = x * 2 in let () = Callback.register "double" double in
    c_function x * 3 in trip x|}
       in
    let env =
      [EnvBinding (false, (ref [((PatVar "x"), tiny_x)]))] @
        ([] @
           [EnvBinding
              (false, (ref [((PatVar "c_function"), c_function_builtin)]))])
       in
    let tiny_result =
      Eval.eval_until_value true false (env @ (!Eval.lib)) program  in
    (Tinyexternal.to_ocaml_value tiny_result : int)
  
let f x =
  let open Tinyocaml in
    let tiny_x = Tinyexternal.of_ocaml_value [] x {|int|}  in
    let (_,program) = Tinyocamlrw.of_string {|let rec f x = double x in f x|}
       in
    let env =
      [EnvBinding (false, (ref [((PatVar "x"), tiny_x)]))] @
        ([] @
           [EnvBinding
              (false, (ref [((PatVar "c_function"), c_function_builtin)]))])
       in
    let tiny_result =
      Eval.eval_until_value true false (env @ (!Eval.lib)) program  in
    (Tinyexternal.to_ocaml_value tiny_result : int)
  
