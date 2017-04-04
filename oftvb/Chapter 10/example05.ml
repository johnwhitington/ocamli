let rec lookup_opt x l =
  match l with
    [] -> None
  | (k, v)::t -> if x = k then Some v else lookup_opt x t
