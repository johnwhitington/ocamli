type 'a mtree = Branch of 'a * 'a mtree list

let rec sum l =
  match l with
    [] -> 0
  | h::t -> h + sum t

let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let rec size tr =
  match tr with
    Branch (e, l) -> 1 + sum (map size l)

let rec total tr =
  match tr with
    Branch (e, l) -> e + sum (map total l)

let rec map_mtree f tr =
  match tr with
    Branch (e, l) -> Branch (f e, map (map_mtree f) l)

let rec size' (Branch (e, l)) =
  1 + sum (map size' l)

let rec total' (Branch (e, l)) =
  e + sum (map total' l)

let rec map_mtree' f (Branch (e, l)) =
  Branch (f e, map (map_mtree' f) l)


