let rec count_true l =
  match l with
    [] -> 0
  | true::t -> 1 + count_true t
  | false::t -> count_true t

