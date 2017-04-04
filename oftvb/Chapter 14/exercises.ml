let round x =
  let c = ceil x in
    let f = floor x in
      if c -. x <= x -. f then c else f

let between (x, y) (x', y') =
  ((x +. x') /. 2., (y +. y') /. 2.)

let rec parts x =
  if x < 0. then
    let a, b = parts (-. x) in
      (-. a, b)
  else
    (floor x, x -. floor x)

let star x =
  let i = int_of_float (floor (x *. 50.)) in
    let i' = if i = 50 then 49 else i in
      for x = 1 to i' - 1 do print_char ' ' done;
      print_char '*';
      print_newline ()

let plot f a b dy =
  let pos = ref a in
    while !pos < b do
      star (f !pos);
      pos := !pos +. dy 
    done
