type 'a tree =
  Lf
| Br of 'a * 'a tree * 'a tree 

let rec member_tree x tr =
  match tr with
    Lf -> false
  | Br (y, l, r) -> x = y || member_tree x l || member_tree x r


