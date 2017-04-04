let rec length l =
  match l with
    [] -> 0
  | _::t -> 1 + length t

let rec append a b =
  match a with
    [] -> b
  | h::t -> h :: append t b
