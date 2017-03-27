let rec even_elements l =
  match l with
    _::b::t -> b :: even_elements t
  | l -> []

