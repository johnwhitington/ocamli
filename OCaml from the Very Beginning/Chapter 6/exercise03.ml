let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let cliplist l =
  map
    (fun x ->
       if x < 1 then 1 else
         if x > 10 then 10 else x)
    l
