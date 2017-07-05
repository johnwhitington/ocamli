let rec for_all f l = 
  match l with 
    [] -> true
  | h::t -> f h && for_all f t
