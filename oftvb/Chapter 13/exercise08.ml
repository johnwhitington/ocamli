let uppercase x =
  if int_of_char x >= 97 && int_of_char x <= 122
    then char_of_int (int_of_char x - 32)
    else x

let lowercase x =
  if int_of_char x >= 65 && int_of_char x <= 90
    then char_of_int (int_of_char x + 32)
    else x
