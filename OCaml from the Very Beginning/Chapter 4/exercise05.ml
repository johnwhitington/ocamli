let rec member e l =
  match l with
    [] -> false
  | h::t -> h = e || member e t
