let rec insert f x l =
  match l with
    [] -> [x]
  | h::t ->
      if f x h
        then x :: h :: t
        else h :: insert f x t

let rec sort f l =
  match l with
    [] -> []
  | h::t -> insert f h (sort f t)
