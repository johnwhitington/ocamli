(*let f a = a

let f a b = a + b

let f a b c = a + b + c

let rec f a = a

let rec f a b = a + b

let rec f a b c = a + b + c

let x = let f a = a in f 1*)

let x = let f a b = a + b in f (1 + 1) (2 + 2)

(*let x = let f a b c = a + b + c in f 1 2 3

let x = let rec f a = a in f 1

let x = let rec f a b = a + b in f 1 2

let x = let rec f a b c = a + b + c in f 1 2 3*)
