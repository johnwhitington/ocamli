type colour = Red | Green | Blue | Yellow

let col = Blue

let cols = [Red; Red; Green; Yellow]

let colpair = ('R', Red)

type colour =
  Red
| Green
| Blue
| Yellow
| RGB of int * int * int

let cols = [Red; Red; Green; Yellow; RGB (150, 0, 255)]

let components c =
  match c with
    Red -> (255, 0, 0)
  | Green -> (0, 255, 0)
  | Blue -> (0, 0, 255)
  | Yellow -> (255, 255, 0)
  | RGB (r, g, b) -> (r, g, b)

let nothing = None

let number = Some 50

let numbers = [Some 12; None; None; Some 2]

let word = Some ['c'; 'a'; 'k'; 'e']

let rec lookup_opt x l =
  match l with
    [] -> None
  | (k, v)::t -> if x = k then Some v else lookup_opt x t

let rec length l =
  match l with
    [] -> 0
  | _::t -> 1 + length t

let rec append a b =
  match a with
    [] -> b
  | h::t -> h :: append t b

type 'a sequence = Nil | Cons of 'a * 'a sequence

let rec length s =
  match s with
    Nil -> 0
  | Cons (_, t) -> 1 + length t

let rec append a b =
  match a with
    Nil -> b
  | Cons (h, t) -> Cons (h, append t b)

type expr =
  Num of int
| Add of expr * expr
| Subtract of expr * expr
| Multiply of expr * expr
| Divide of expr * expr

let rec evaluate e =
  match e with
    Num x -> x
  | Add (e, e') -> evaluate e + evaluate e'
  | Subtract (e, e') -> evaluate e - evaluate e'
  | Multiply (e, e') -> evaluate e * evaluate e'
  | Divide (e, e') -> evaluate e / evaluate e'
