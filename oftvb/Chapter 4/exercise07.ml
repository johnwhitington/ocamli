let rec rev l =
  match l with
    [] -> []
  | h::t -> rev t @ [h]

let rec drop_last_inner a l =
  match l with
    [] -> rev a
  | [_] -> rev a
  | h::t -> drop_last_inner (h :: a) t

let drop_last l =
  drop_last_inner [] l

