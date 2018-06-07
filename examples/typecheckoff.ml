let g x = x + 1

let rec map f = function
  | [] -> []
  | h::t -> f h :: map t

let l = map f [1; 2; 3]

