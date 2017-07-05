let rec length l =
  match l with
    [] -> 0
  | h::t -> 1 + length t
