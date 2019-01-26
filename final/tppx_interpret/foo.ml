let[@showtype] f (x [@showtype]) y = x + y [@showtype "result"]

(*let[@showtype] rec merge (x [@showtype]) y =
  match x, y [@showtype] with
    [], l -> l
  | l, [] -> l
  | hx::tx, hy::ty ->
      if hx < hy
        then hx :: merge tx (hy :: ty)
        else hy :: merge (hx :: tx) ty

let rec take n l =
  if n = 0 [@showtype] then [] else
    match l with
      h::t -> h :: take (n - 1) t
    | [] -> failwith "take"

let rec drop n l =
  if n = 0 then l else
    match l with
      h::t -> drop (n - 1) t
    | [] -> failwith "drop"

let rec msort l =
  match l with
    [] -> []
  | [x] -> [x]
  | _ ->
      let left = take (List.length l / 2) l
      and right = drop (List.length l / 2) l in
        merge (msort left) (msort right)*)

