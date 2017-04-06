let rec even_elements l =
  match l with
    [] -> []
  | [_] -> []
  | _::b::t -> b :: even_elements t

let rec even_elements' l =
  match l with
    _::b::t -> b :: even_elements' t
  | l -> []
