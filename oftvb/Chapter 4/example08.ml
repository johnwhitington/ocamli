let rec append a b =
  match a with
    [] -> b
  | h::t -> h :: append t b
