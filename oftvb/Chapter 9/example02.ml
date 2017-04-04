let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let mapl f l = map (map f) l

let mapl f = map (map f)
