let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let firstelt n l =
  match l with [] -> n | h::_ -> h

let firstelts n l =
  map (firstelt n) l
