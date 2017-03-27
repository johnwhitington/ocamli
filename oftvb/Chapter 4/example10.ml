let rec take n l =
  if n = 0 then [] else
    match l with
      h::t -> h :: take (n - 1) t
