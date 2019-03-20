let rec pairs f a = function
  [] -> List.rev a
| [_] -> []
| h::h'::t -> pairs f (f h h'::a) (h'::t)

let x = (pairs ( + ) [] [1; 2; 3; 4] [@interpret])

