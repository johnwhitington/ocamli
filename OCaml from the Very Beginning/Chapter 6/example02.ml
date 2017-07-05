let rec evens l =
  match l with
    [] -> []
  | h::t -> (h mod 2 = 0) :: evens t
