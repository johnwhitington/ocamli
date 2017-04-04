let rec lookup x l =
  match l with
    [] -> raise Not_found
  | (k, v)::t ->
      if k = x then v else lookup x t

let rec key_exists k d =
  try
    let _ = lookup k d in true
  with
    Not_found -> false
