(* This the module we wish to interpret *)

(* The marker to say this file should be interpreted *)
[%interpret]

(* An external function implemented in C *)
external c_function : int -> int = "c_function"

(* Here, a function which calls something in another module. *)
(*let double x = A.double x*)

(* A simple function, using the C function *)
let trip x =
  let double x = x * 2 in
  (* Use Callback.register, so c_function can call back to double. When values done, move to top level. *)
  let () = Callback.register "double" double in
    c_function x * 3 

(* Here, a function which calls something in this module *)
(*let f x = double x*)

