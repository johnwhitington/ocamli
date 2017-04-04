type 'a tree =
  Lf
| Br of 'a * 'a tree * 'a tree 

let rec equal_shape tr tr2 =
  match tr, tr2 with
    Lf, Lf ->
      true
  | Br (_, l, r), Br (_, l2, r2) ->
      equal_shape l l2 && equal_shape r r2
  | _, _ ->
      false

let rec tree_map f tr =
  match tr with
    Br (x, l, r) -> Br (f x, tree_map f l, tree_map f r)
  | Lf -> Lf

let rec equal_shape' tr tr2 =
  tree_map (fun _ -> 0) tr = tree_map (fun _ -> 0) tr2


