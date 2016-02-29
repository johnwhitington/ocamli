(* Swap the contents of two references *)
let x = ref 0 in
  let y = ref 1 in
    let t = !x in
      x := !y;
      y := t
