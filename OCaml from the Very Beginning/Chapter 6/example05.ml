let greater a b =
  a >= b

let rec merge cmp x y =
  match x, y with
    [], l -> l
  | l, [] -> l
  | hx::tx, hy::ty ->
      if cmp hx hy
        then hx :: merge cmp tx (hy :: ty)
        else hy :: merge cmp (hx :: tx) ty
