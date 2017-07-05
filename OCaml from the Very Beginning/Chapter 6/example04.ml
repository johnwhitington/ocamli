let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let evens l =
  map (fun x -> x mod 2 = 0) l
