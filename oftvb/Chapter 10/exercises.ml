type rect =
    Square of int
  | Rectangle of int * int

let area r =
  match r with
    Square s -> s * s
  | Rectangle (w, h) -> w * h

let rotate r =
  match r with
    Rectangle (w, h) ->
      if w > h then Rectangle (h, w) else r
  | Square _ -> r

let width_of_rect r =
  match r with
    Square s -> s
  | Rectangle (w, _) -> w

let rect_compare a b =
  width_of_rect a < width_of_rect b

let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let rec insert f x l =
  match l with
    [] -> [x]
  | h::t ->
      if f x h
        then x :: h :: t
        else h :: insert f x t

let rec sort f l =
  match l with
    [] -> []
  | h::t -> insert f h (sort f t)

let pack rs =
  sort rect_compare (map rotate rs)

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

let rec power x n =
  if n = 0 then 1 else
    if n = 1 then x else
      x * power x (n - 1)

type expr =
    Num of int
  | Add of expr * expr
  | Subtract of expr * expr
  | Multiply of expr * expr
  | Divide of expr * expr
  | Power of expr * expr

let rec evaluate e =
  match e with
    Num x -> x
  | Add (e, e') -> evaluate e + evaluate e'
  | Subtract (e, e') -> evaluate e - evaluate e'
  | Multiply (e, e') -> evaluate e * evaluate e'
  | Divide (e, e') -> evaluate e / evaluate e'
  | Power (e, e') -> power (evaluate e) (evaluate e')

let evaluate_opt e =
  try Some (evaluate e) with Division_by_zero -> None
