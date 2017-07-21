let _ = Pptinyocaml.simple := true; Ocamliutil.typecheck := false 
let exception_from_ocaml e = Tinyocaml.Unit 
[%%auto {|external c_function : int -> int = "c_function"|}]
let c_function_builtin = snd c_function 
let trip x = x * 3 
let _ = (Callback.register "trip") trip 
let quad x = c_function ((A.double x) * 2) 
