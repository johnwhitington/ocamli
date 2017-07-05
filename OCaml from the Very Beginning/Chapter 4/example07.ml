let rec odd_elements l =
  match l with
    a::_::t -> a :: odd_elements t
  | _ -> l

