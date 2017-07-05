type t = A of {x : int; mutable y : float}

let p = A {x = 5; y = 6.5}

let r = match p with A {x = xx; y = yy} -> xx

