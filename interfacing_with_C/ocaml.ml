(* This is the main ocaml file. *)

(* We register a callback, so that the C function can call an OCaml one. *)
let write () = print_string "CALLED BACK FROM C\n"

let _ = Callback.register "write" write

(* Here is a function in C we call into *)
external cfunction : int -> int = "f"

let _ =
  print_int (cfunction 42);
  print_string "\n"



let o = 1

