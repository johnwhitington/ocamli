let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let rec mapl f l =
  match l with
    [] -> []
  | h::t -> map f h :: mapl f t
