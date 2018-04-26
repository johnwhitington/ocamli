(* This type has a type variable to allow us to represent pointers into the heap *)
type op = Add | Sub | Mul | Div

type 'a t =
  Unit
| Int of int
| Bool of bool
| Value of 'a       (* All boxed values e.g floats, arrays. *)
| ArrayExpr of 'a t array (* Array expression which is not a value e.g [|1 + 2; 3|] *)
| FOp of (op * 'a t * 'a t)
| ArrayGet of ('a t * 'a t) (* arr, index *)
| ArraySet of ('a t * 'a t * 'a t) (* arr, index, newval *)


