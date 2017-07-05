let rec parts x =
  if x < 0. then
    let a, b = parts (-. x) in
      (-. a, b)
  else
    (floor x, x -. floor x)
