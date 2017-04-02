let rec apply f n x =
  if n = 0
    then x
    else f (apply f (n - 1) x)

let power a b =
  apply (fun x -> x * a) b 1
