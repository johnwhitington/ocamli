let rec calm l =
  match l with
    [] -> []
  | '!'::t -> '.' :: calm t
  | h::t -> h :: calm t

let calm_char x =
  match x with '!' -> '.' | _ -> x

let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let calm l =
  map calm_char l
