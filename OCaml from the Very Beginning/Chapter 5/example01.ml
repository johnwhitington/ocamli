let rec insert x l =
  match l with
    [] -> [x]
  | h::t ->
      if x <= h
        then x :: h :: t
        else h :: insert x t

let rec sort l =
  match l with
    [] -> []
  | h::t -> insert h (sort t)
