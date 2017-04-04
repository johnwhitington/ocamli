type 'a sequence = Nil | Cons of 'a * 'a sequence

let rec take n l =
  if n = 0 then Nil else
    match l with
      Nil -> raise (Invalid_argument "take")
    | Cons (h, t) -> Cons (h, take (n - 1) t)

let rec drop n l =
  if n = 0 then l else
    match l with
      Nil -> raise (Invalid_argument "drop")
    | Cons (_, l) -> drop (n - 1) l

let rec map f l =
  match l with
    Nil -> Nil
  | Cons (h, t) -> Cons (f h, map f t)
