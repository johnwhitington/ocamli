let rec length l =
  match l with
    [] -> 0
  | _::t -> 1 + length t
