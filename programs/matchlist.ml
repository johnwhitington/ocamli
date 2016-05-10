let tl l = match l with _::t -> t | _ -> []

let x = tl [1; 2; 3]

