let rec lookup x l =
  match l with
    [] -> raise Not_found
  | (k, v)::t ->
      if k = x then v else lookup x t
