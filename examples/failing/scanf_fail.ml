let g =
  let x = 1 in (fun _ _ -> x)

let i () = g 0

let _ = i ()

