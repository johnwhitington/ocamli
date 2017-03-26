let rec factorial a =
  match a with
    1 -> a
  | _ -> a * factorial (a - 1)
