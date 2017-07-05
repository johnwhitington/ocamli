type 'a tree =
    Br of 'a * 'a tree * 'a tree
  | Lf

let rec lookup tr k =
  match tr with
    Lf -> raise Not_found
  | Br ((k', v), l, r) ->
      if k = k' then v
      else if k < k' then lookup l k
      else lookup r k

