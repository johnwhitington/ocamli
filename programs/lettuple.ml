(*let rec split = function
    [] -> ([], [])
  | (x,y)::l ->
      let (rx, ry) = split l in (x::rx, y::ry)*)

let p = 
  let (x, y) = (1, 2) in
    x + y
