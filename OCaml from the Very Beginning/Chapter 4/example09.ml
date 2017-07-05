let rec rev l =
  match l with
    [] -> []
  | h::t -> rev t @ [h]
