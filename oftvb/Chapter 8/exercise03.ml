let rec mkdict keys values =
  match keys, values with
    [], [] -> []
  | _, [] -> raise (Invalid_argument "mkdict")
  | [], _ -> raise (Invalid_argument "mkdict")
  | k::ks, v::vs -> (k, v) :: mkdict ks vs
