type rect =
    Square of int
  | Rectangle of int * int

let rotate r =
  match r with
    Rectangle (w, h) ->
      if w > h then Rectangle (h, w) else r
  | Square _ -> r
