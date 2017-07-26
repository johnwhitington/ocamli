(* This the module we wish to interpret *)

(* The marker to say this file should be interpreted *)
[%interpret]

(* An external function implemented in C *)
external c_function : int -> int = "c_function"

(* Here, a function which calls something in another module. *)
let double x = A.double x

(* Use Callback.register, so c_function can call back to double. *)
let () = Callback.register "double" double

(* A simple function, using the C function *)
let trip x = c_function x * 3 

(* Here, a function which calls something in this module *)
let f x = double x

