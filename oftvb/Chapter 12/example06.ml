(*1*)
let print_dict_entry (k, v) =
  print_int k ; print_newline () ; print_string v ; print_newline ()

(*2*)
let print_dict_entry (k, v) =
  print_int k;
  print_newline ();
  print_string v;
  print_newline ()

(*3*)
let rec print_dict d =
  match d with
    [] -> ()
  | h::t -> print_dict_entry h; print_dict t

(*4*)
let rec iter f l =
  match l with
    [] -> ()
  | h::t -> f h; iter f t

let print_dict d =
  iter print_dict_entry d

let print_dict' =
  iter print_dict_entry

(*5*)
let rec read_dict () =
  let i = read_int () in
    if i = 0 then [] else
      let name = read_line () in
        (i, name) :: read_dict ()

(*6*)
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

(*6*)
let entry_to_channel ch (k, v) =
  output_string ch (string_of_int k);
  output_char ch '\n';
  output_string ch v;
  output_char ch '\n'

let dictionary_to_channel ch d =
  iter (entry_to_channel ch) d

let dictionary_to_file filename dict =
  let ch = open_out filename in
    dictionary_to_channel ch dict;
    close_out ch

(*7*)
let entry_of_channel ch =
  let number = input_line ch in
    let name = input_line ch in
      (int_of_string number, name)

let rec dictionary_of_channel ch =
  try
    let e = entry_of_channel ch in
      e :: dictionary_of_channel ch
  with
    End_of_file -> []

let dictionary_of_file filename =
  let ch = open_in filename in
    let dict = dictionary_of_channel ch in
      close_in ch;
      dict

