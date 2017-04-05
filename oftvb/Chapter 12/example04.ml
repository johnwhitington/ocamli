let rec iter f l =
  match l with
    [] -> ()
  | h::t -> f h; iter f t

let print_dict d =
  iter print_dict_entry d

let print_dict' =
  iter print_dict_entry

