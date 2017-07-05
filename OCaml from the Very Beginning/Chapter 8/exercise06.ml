let rec add k v d =
  match d with
    [] -> [(k, v)]
  | (k', v')::t ->
      if k = k'
        then (k, v) :: t
        else (k', v') :: add k v t

let rec union a b =
  match a with
    [] -> b
  | (k, v)::t -> add k v (union t b)
