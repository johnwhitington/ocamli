(* 1 Immutable value *)
(*let x = 1 + 2 + 3

(* x is free in f *)
let f y = x + y

let _ = f 0*)

(*let _ = f 1*)

(* 2. Mutable value *)
let x = ref (5 + 4)

let f y =
  x := !x + y; !x

let _ = f 0

