type op = Add | Sub | Mul | Div

type t' =
  Value of Obj.t
| ArrayExpr of t array (* Array not yet a value e.g [|1 + 2; 3|] *)
| IntOp of (op * t * t)
| FOp of (op * t * t)
| ArrayGet of (t * t)
| ArraySet of (t * t * t)

and t =
  {typ : string;
   e : t'}

