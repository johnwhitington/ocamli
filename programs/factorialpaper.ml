let rec factorial n =
  if n = 1 then 1 else n * factorial (n - 1)
in
  print_int (factorial (int_of_string (input_line stdin)))

