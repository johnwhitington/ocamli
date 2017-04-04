let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let rec member e l =
  match l with
    [] -> false
  | h::t -> h = e || member e t

let member_all x ls =
  let booleans = map (member x) ls in
    not (member false booleans)

let member_all x ls =
  not (member false (map (member x) ls))

