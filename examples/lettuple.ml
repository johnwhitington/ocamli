let rec split = function
    [] -> ([], [])
  | (x,y)::l ->
      let (rx, ry) = split l in (x::rx, y::ry)

let _ = split [(1, 2); (3, 4); (5, 6)]

