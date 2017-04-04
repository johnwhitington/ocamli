let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let mapll f l = map (map (map f)) l

let mapll f = map (map (map f))
