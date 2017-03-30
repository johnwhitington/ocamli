let rec merge x y =
  match x, y with
    [], l -> l
  | l, [] -> l
  | hx::tx, hy::ty ->
      if hx < hy
        then hx :: merge tx (hy :: ty)
        else hy :: merge (hx :: tx) ty
