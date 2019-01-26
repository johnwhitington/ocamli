let rec f x = x + 1
and g y = if y = 10 then 0 else f y

let p = g 0

