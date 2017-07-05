let rec sum_match n =
  match n with
    1 -> 1
  | _ -> n + sum_match (n - 1)
