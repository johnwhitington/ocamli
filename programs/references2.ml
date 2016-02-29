(* Function to swap the contents of two references *)
let swap x y =
  let t = !x in
    x := !y;
    y := t

let x = ref false
let y = ref true

let () = swap x y

