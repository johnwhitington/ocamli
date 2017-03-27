let rec length_inner l n =
  match l with
    [] -> n
  | h::t -> length_inner t (n + 1)

let length l = length_inner l 0
