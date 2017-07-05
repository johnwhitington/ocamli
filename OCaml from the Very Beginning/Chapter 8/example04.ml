let rec remove k d =
  match d with
    [] -> []
  | (k', v')::t ->
      if k = k'
        then t
        else (k', v') :: remove k t
