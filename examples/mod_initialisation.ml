(* 2. Mutable value *)
let x = ref (5 + 4)

let f y =
  x := !x + y; !x

let _ = f 0

