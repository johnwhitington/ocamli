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

let rec msort l =
  match l with
    [] -> []
  | [x] -> [x]
  | _ ->
      let left = take (length l / 2) l
      and right = drop (length l / 2) l in
        merge (msort left) (msort right)
