let isupper c =
  match c with
    'A'..'Z' -> true
  | _ -> false

let islower c =
  not (isupper c)