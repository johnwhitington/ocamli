let print_dict_entry (k, v) =
  print_int k ; print_newline () ; print_string v ; print_newline ()

let rec print_dict d =
  match d with
    [] -> ()
  | h::t -> print_dict_entry h; print_dict t

