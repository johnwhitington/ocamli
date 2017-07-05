let rec rev l =
  match l with
    [] -> []
  | h::t -> rev t @ [h]

let mk_palindrome l =
  l @ rev l

let is_palindrome l = 
  l = rev l

