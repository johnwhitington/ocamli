let x = [|1; 2; 3|]

let y = x.(0)

let p = match x with [|x; y; z|] -> x + y + z

