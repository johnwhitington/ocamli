let rec map f = function
    [] -> []
  | a::l -> let r = f a in r :: map f l

