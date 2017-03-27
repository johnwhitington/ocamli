let rec count_true_inner n l =
  match l with
    [] -> n
  | true::t -> count_true_inner (n + 1) t
  | false::t -> count_true_inner n t

let count_true l =
  count_true_inner 0 l

