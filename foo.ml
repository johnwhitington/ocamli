let find x = raise Not_found

let x =
  let rec find accu = function
    | [] -> []
    | h::t -> find 0 t
  in
    find 0 [1]
