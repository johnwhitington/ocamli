let rec double l =
  match l with
    [] -> []
  | h::t -> (h * 2) :: double t
