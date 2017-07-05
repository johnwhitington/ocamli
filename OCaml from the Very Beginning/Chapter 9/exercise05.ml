let rec length l =
  match l with
    [] -> 0
  | _::t -> 1 + length t

let rec take n l =
  if n = 0 then [] else
    match l with
      h::t -> h :: take (n - 1) t

let truncate_l n l =
  if length l >= n then take n l else l

let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let truncate n ll =
  map (truncate_l n) ll
