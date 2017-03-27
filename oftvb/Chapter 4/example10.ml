let isnil l =
  match l with
    [] -> true
  | _ -> false

let rec length l =
  match l with
    [] -> 0
  | h::t -> 1 + length t

let rec length l =
  match l with
    [] -> 0
  | _::t -> 1 + length t

let rec sum l =
  match l with
    [] -> 0
  | h::t -> h + sum t

let rec length_inner l n =
  match l with
    [] -> n
  | h::t -> length_inner t (n + 1)

let length l = length_inner l 0

let rec odd_elements l =
  match l with
    [] -> []
  | [a] -> [a]
  | a::_::t -> a :: odd_elements t

let rec odd_elements l =
  match l with
    a::_::t -> a :: odd_elements t
  | _ -> l

let rec append a b =
  match a with
    [] -> b
  | h::t -> h :: append t b
 
let rec rev l =
  match l with
    [] -> []
  | h::t -> rev t @ [h]

let rec take n l =
  if n = 0 then [] else
    match l with
      h::t -> h :: take (n - 1) t

let rec drop n l =
  if n = 0 then l else
    match l with
      h::t -> drop (n - 1) t
