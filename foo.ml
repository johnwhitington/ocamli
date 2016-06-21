let f =
  let rec g = function [] -> [] | _::l -> g l in
    g

let x = f [0]

