type rect =
    Square of int
  | Rectangle of int * int

let rotate r =
  match r with
    Rectangle (w, h) ->
      if w > h then Rectangle (h, w) else r
  | Square _ -> r

let width_of_rect r =
  match r with
    Square s -> s
  | Rectangle (w, _) -> w

let rect_compare a b =
  width_of_rect a < width_of_rect b

let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let rec insert f x l =
  match l with
    [] -> [x]
  | h::t ->
      if f x h
        then x :: h :: t
        else h :: insert f x t

let rec sort f l =
  match l with
    [] -> []
  | h::t -> insert f h (sort f t)

let pack rs =
  sort rect_compare (map rotate rs)
