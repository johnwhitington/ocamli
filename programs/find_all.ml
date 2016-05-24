(*let rec find p = function
  | [] -> raise Not_found
  | x :: l -> if p x then x else find p l*)

let find_all =
  let rec find accu = function
  | [] -> accu
  | x :: l -> find accu l
  in
    find []

let x = find_all [0]

