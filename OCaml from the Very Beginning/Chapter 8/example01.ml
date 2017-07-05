let p = (1, 4)

let q = (1, '1')

let fst p = match p with (x, _) -> x

let snd p = match p with (_, y) -> y

let fst (x, _) = x

let snd (_, y) = y

let census = [(1, 4); (2, 2); (3, 2); (4, 3); (5, 1); (6, 2)]

let y = (1, [2; 3; 4])
