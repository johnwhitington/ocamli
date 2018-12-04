(* The buggy version, for demonstration purposes *)
let rev = function
  [] -> []
| h::t -> rev t @ [h]

let rec pairs f a l =
  match l with 
    [] -> rev a
  | [_] -> []
  | h::h'::t -> pairs f (f h h' :: a) t

let x = pairs ( + ) [] [1; 2; 3; 4]
