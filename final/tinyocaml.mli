type op = Add | Sub | Mul | Div

type 'a t' =
  Value of 'a
| ArrayExpr of 'a t array (* Array not yet a value e.g [|1 + 2; 3|] *)
| IntOp of (op * 'a t * 'a t)
| FOp of (op * 'a t * 'a t)
| ArrayGet of ('a t * 'a t)
| ArraySet of ('a t * 'a t * 'a t)

and 'a t =
  {typ : string;
   expr : 'a t'}

