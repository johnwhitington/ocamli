let rec member e l =
  match l with
    [] -> false
  | h::t -> h = e || member e t

let rec make_set l =
  match l with
    [] -> []
  | h::t -> if member h t then make_set t else h :: make_set t

