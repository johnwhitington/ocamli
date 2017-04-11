let n = ref 0

let _ = (fun x -> n := 32) ()

let x = n

(*let _ = x := 42
let _ = n := 10*)
