let rec replace k v l =
  match l with
    [] -> raise Not_found
  | (k', v')::t ->
      if k = k'
        then (k, v) :: t
        else (k', v') :: replace k v t
