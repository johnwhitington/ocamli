type 'a tree =
  Lf
| Br of 'a * 'a tree * 'a tree 

let rec insert tr k v =
  match tr with
    Lf -> Br ((k, v), Lf, Lf)
  | Br ((k', v'), l, r) ->
      if k = k' then Br ((k, v), l, r)
      else if k < k' then Br ((k', v'), insert l k v, r)
      else Br ((k', v'), l, insert r k v)

let rec tree_of_list l =
  match l with
    [] -> Lf
  | (k, v)::t -> insert (tree_of_list t) k v

let rec list_of_tree tr =
  match tr with
    Br (x, l, r) -> list_of_tree l @ [x] @ list_of_tree r
  | Lf -> []

let tree_union t t' =
  tree_of_list (list_of_tree t' @ list_of_tree t)


