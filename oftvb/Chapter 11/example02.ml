type 'a tree =
    Br of 'a * 'a tree * 'a tree
  | Lf

let max x y =
  if x > y then x else y

let rec maxdepth tr =
  match tr with
    Br (_, l, r) -> 1 + max (maxdepth l) (maxdepth r)
  | Lf -> 0

