let rec print_dict d =
  match d with
    [] -> ()
  | h::t -> print_dict_entry h; print_dict t

