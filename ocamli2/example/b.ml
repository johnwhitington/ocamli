(* This the module we wish to interpret *)

(* The marker to say this file should be interpreted *)
(*[%interpret]*)

(* An external function implemented in C. It will call back into OCaml too. *)
(*external c_function : int -> int = "c_function"*)

(* A recursive function, which also uses something from another module *)
let rec double x =
  if x < 100 then double (x * 2) else A.double x

(* A simple function, using the C function *)
(*let trip x =
  let double x = x * 2 in
  (* Use Callback.register, so c_function can call back to double. When values done, move to top level. *)
  let () = Callback.register "double" double in
    c_function x * 3*)

(* Here, a function which calls something in this module *)
let f x = double x

