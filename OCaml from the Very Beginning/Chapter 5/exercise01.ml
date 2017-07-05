let rec take n l =
  if n = 0 then [] else
    match l with
      h::t -> h :: take (n - 1) t

let rec drop n l =
  if n = 0 then l else
    match l with
      h::t -> drop (n - 1) t

let rec length x =
  match x with
    [] -> 0
  | _::t -> 1 + length t

let rec merge x y =
  match x, y with
    [], l -> l
  | l, [] -> l
  | hx::tx, hy::ty ->
      if hx < hy
        then hx :: merge tx (hy :: ty)
        else hy :: merge (hx :: tx) ty

let rec msort l =
  match l with
    [] -> []
  | [x] -> [x]
  | _ ->
      let x = length l / 2 in
        let left = take x l
        and right = drop x l in
          merge (msort left) (msort right)

