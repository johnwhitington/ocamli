type t = A of {x : int; y : float}

let p = A {x = 5; y = 6.5}

let _ = match p with A q -> q.x

