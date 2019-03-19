let global = function x -> x * 2

let x =
  let y = 1 - 0 in
    (Random.int 50 + y + 2 * 3 + global 42 [@interpret]) + 4

let _ =
  Printf.printf "Result is %i\n" x
