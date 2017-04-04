let rec flip_tree tr =
  match tr with
    Lf -> Lf
  | Br (x, l, r) -> Br (x, flip_tree r, flip_tree l)


