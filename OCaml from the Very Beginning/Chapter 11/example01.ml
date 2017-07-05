type 'a tree =
    Br of 'a * 'a tree * 'a tree
  | Lf

let rec size tr =
  match tr with
    Br (_, l, r) -> 1 + size l + size r
  | Lf -> 0

let rec total tr =
  match tr with
    Br (x, l, r) -> x + total l + total r
  | Lf -> 0

