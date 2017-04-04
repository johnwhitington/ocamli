type 'a tree =
    Br of 'a * 'a tree * 'a tree
  | Lf

let rec lookup tr k =
  match tr with
    Lf -> None
  | Br ((k', v), l, r) ->
      if k = k' then Some v
      else if k < k' then lookup l k
      else lookup r k

let rec insert tr k v =
  match tr with
    Lf -> Br ((k, v), Lf, Lf)
  | Br ((k', v'), l, r) ->
      if k = k' then Br ((k, v), l, r)
      else if k < k' then Br ((k', v'), insert l k v, r)
      else Br ((k', v'), l, insert r k v)

