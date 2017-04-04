type rect =
    Square of int
  | Rectangle of int * int

let area r =
  match r with
    Square s -> s * s
  | Rectangle (w, h) -> w * h
