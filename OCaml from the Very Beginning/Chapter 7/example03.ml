let rec last l =
  match l with
    [x] -> x
  | _::t -> last t
