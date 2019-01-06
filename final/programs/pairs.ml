(* The buggy version, for demonstration purposes *)
(*let rec rev = function
  [] -> []
| h::t -> rev t @ [h]

let rec pairs f a l =
  match l with 
    [] -> rev a
  | [_] -> []
  | h::h'::t -> pairs f (f h h' :: a) t

let x = pairs ( + ) [] [1; 2; 3; 4]*)
let f x y z = x + y + z

let p = f 1 2 3

