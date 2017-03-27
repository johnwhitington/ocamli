let rec even_elements l =
  match l with
    [] -> []
  | [_] -> []
  | _::b::t -> b :: even_elements t

