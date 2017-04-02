let rec mapl f l =
  match l with
    [] -> []
  | h::t -> map f h :: mapl f t
