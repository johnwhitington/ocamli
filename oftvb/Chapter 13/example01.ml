let swap a b =
  let t = !a in
    a := !b; b := t
