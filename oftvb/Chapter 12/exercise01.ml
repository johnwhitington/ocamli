let rec iter f l =
  match l with
    [] -> ()
  | h::t -> f h; iter f t

let print_integers l =
  print_string "[";
  iter (fun i -> print_int i; print_string "; ") l;
  print_string "]"

let rec print_integers_inner l =
  match l with
    [] -> ()
  | [i] -> print_int i
  | h::t -> print_int h; print_string "; "; print_integers_inner t

let print_integers l =
  print_string "[";
  print_integers_inner l;
  print_string "]"
