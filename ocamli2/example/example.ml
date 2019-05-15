let global x = x * 2

let x () =
  let y = 3 - 4 in
    (Random.int 50 + y + 5 + global 6 [@interpret]) + 7

let _ =
  Printf.printf "Result is %i\n" (x ())

