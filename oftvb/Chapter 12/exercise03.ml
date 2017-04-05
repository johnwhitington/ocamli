let rec read_dict_number n =
  if n = 0 then [] else
    try
      let i = read_int () in
        let name = read_line () in
          (i, name) :: read_dict_number (n - 1)
    with
      Failure "int_of_string" ->
        print_string "This is not a valid integer.";
        print_newline ();
        print_string "Please enter integer and name again.";
        print_newline ();
        read_dict_number n
