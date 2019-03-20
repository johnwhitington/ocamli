let rec pairs f a = function
  [] | [_] -> List.rev a
| h::h'::t -> pairs f (f h h'::a) (h'::t)

let x = pairs ( + ) [] [1; 2; 3; 4]

