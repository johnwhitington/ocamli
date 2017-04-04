type 'a tree =
    Br of 'a * 'a tree * 'a tree
  | Lf

let rec tree_map f tr =
  match tr with
    Br (x, l, r) -> Br (f x, tree_map f l, tree_map f r)
  | Lf -> Lf

