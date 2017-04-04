let rec last l =
  match l with
    [] -> raise Not_found
  | [x] -> x
  | _::t -> last t
