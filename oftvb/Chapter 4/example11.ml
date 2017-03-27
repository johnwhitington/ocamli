let rec drop n l =
  if n = 0 then l else
    match l with
      h::t -> drop (n - 1) t
