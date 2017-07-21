let _ = Pptinyocaml.simple := true; Ocamliutil.typecheck := false 
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
let trip x = x * 3 
let _ = (Callback.register "trip") trip 
let quad x = c_function ((A.double x) * 2) 
