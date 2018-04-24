let rec factorial a n =
  if n = 1 then a else factorial (a * n) (n - 1)
in
  factorial 1 4

