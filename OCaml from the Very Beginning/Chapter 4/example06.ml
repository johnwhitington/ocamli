let rec odd_elements l =
  match l with
    [] -> []
  | [a] -> [a]
  | a::_::t -> a :: odd_elements t

