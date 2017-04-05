let array_sum a =
  let sum = ref 0 in
    for x = 0 to Array.length a - 1 do
      sum := !sum + a.(x)
    done;
    !sum
