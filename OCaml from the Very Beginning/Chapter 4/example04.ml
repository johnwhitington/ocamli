let rec sum l =
  match l with
    [] -> 0
  | h::t -> h + sum t
