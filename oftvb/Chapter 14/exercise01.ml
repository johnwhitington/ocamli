let round x =
  let c = ceil x in
    let f = floor x in
      if c -. x <= x -. f then c else f
