let star x =
  let i = int_of_float (floor (x *. 50.)) in
    let i' = if i = 50 then 49 else i in
      for x = 1 to i' - 1 do print_char ' ' done;
      print_char '*';
      print_newline ()

let plot f a b dy =
  let pos = ref a in
    while !pos < b do
      star (f !pos);
      pos := !pos +. dy 
    done
