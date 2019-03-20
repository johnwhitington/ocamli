let rec pairs f a = function
  h::h'::t -> pairs f (f h h'::a) (h'::t)
| _ -> List.rev a


let x = pairs ( + ) [] [1; 2; 3; 4]

