let array_rev a =
  if a <> [||] then
    for x = 0 to Array.length a / 2 do
      let t = a.(x) in
        a.(x) <- a.(Array.length a - 1 - x);
        a.(Array.length a - 1 - x) <- t
    done
