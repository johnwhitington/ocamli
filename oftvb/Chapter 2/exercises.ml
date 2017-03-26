let times_ten x = x * 10

let both_non_zero x y =
  x <> 0 && y <> 0

let rec sum n =
  if n = 1 then 1 else n + sum (n - 1)

let rec power x n =
  if n = 0 then 1 else
    (if n = 1 then x else
       x * power x (n - 1))

let rec power x n =
  if n = 0 then 1 else
    if n = 1 then x else
      x * power x (n - 1)

let isvowel c =
  c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u'

let isconsonant c =
  not (isvowel c)

let rec factorial x =
  if x <= 0 then 0 else
    if x = 1 then 1 else
      x * factorial (x - 1)
