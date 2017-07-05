let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let is_even x =
  x mod 2 = 0

let evens l =
  map is_even l
