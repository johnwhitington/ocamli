let rec is_sorted l =
  match l with
    a::b::t -> a <= b && is_sorted (b :: t)
  | _ -> true

