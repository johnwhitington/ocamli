let rec mklists l =
  match l with
    [] -> ([], [])
  | (k, v)::more ->
      match mklists more with
        (ks, vs) -> (k :: ks, v :: vs)
