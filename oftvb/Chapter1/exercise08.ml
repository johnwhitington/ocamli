let rec factorial x =
  if x <= 0 then 0 else
    if x = 1 then 1 else
      x * factorial (x - 1)
