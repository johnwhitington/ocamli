type 'a tree =
    Br of 'a * 'a tree * 'a tree
  | Lf

let rec list_of_tree tr =
  match tr with
    Br (x, l, r) -> list_of_tree l @ [x] @ list_of_tree r
  | Lf -> []

