let x = ref 3 in
  while !x > 0 do print_string "foo\n"; x := !x - 1 done
