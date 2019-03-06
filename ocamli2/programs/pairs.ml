(* The buggy version, for demonstration purposes *)
let rec pairs f a = function
| h::t -> pairs f (f h :: a) t
| _ -> []

let x = pairs ( + ) [] [1]

