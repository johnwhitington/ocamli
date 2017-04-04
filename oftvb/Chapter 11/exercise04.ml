(*1*)
type 'a tree =
  Lf
| Br of 'a * 'a tree * 'a tree 

let rec member_tree x tr =
  match tr with
    Lf -> false
  | Br (y, l, r) -> x = y || member_tree x l || member_tree x r

(*2*)
let rec flip_tree tr =
  match tr with
    Lf -> Lf
  | Br (x, l, r) -> Br (x, flip_tree r, flip_tree l)

(*3*)
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

(*4*)
let rec insert tr k v =
  match tr with
    Lf -> Br ((k, v), Lf, Lf)
  | Br ((k', v'), l, r) ->
      if k = k' then Br ((k, v), l, r)
      else if k < k' then Br ((k', v'), insert l k v, r)
      else Br ((k', v'), l, insert r k v)

let rec list_of_tree tr =
  match tr with
    Br (x, l, r) -> list_of_tree l @ [x] @ list_of_tree r
  | Lf -> []

let rec tree_of_list l =
  match l with
    [] -> Lf
  | (k, v)::t -> insert (tree_of_list t) k v

(*5*)
let tree_union t t' =
  tree_of_list (list_of_tree t' @ list_of_tree t)

(*6*)
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


