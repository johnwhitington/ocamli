let rec read_dict () =
  try
    let i = read_int () in
      if i = 0 then [] else
        let name = read_line () in
          (i, name) :: read_dict ()
  with
    Failure "int_of_string" ->
      print_string "This is not a valid integer. Please try again.";
      print_newline ();
      read_dict ()

