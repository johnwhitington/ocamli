let rec numlist n =
  match n with
    0 -> []
  | _ -> (numlist (n - 1)) @ [n]  

let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let rec iter f l =
  match l with
    [] -> ()
  | h::t -> f h; iter f t

let write_table_channel ch n =
  iter
    (fun x ->
       iter
          (fun i ->
             output_string ch (string_of_int i);
             output_string ch "\t")
          (map (( * ) x) (numlist n));
       output_string ch "\n")
    (numlist n)

exception FileProblem

let table filename n =
  if n < 0 then raise (Invalid_argument "table") else
    try
      let ch = open_out filename in
        write_table_channel ch n;
        close_out ch
    with
      _ -> raise FileProblem
