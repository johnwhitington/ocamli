(* The buggy version, for demonstration purposes *)
let rec rev = function
  [] -> []
| h::t -> rev t @ [h]

let x = rev [1]

let rec pairs f a = function
  [] -> rev a
| [_] -> []
| h::h'::t -> pairs f (f h h' :: a) t

let x = pairs ( + ) [] [1; 2; 3; 4]

