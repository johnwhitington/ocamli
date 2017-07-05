let f ~x ?(y = 0) ?z = x - y

let g = 1 + f ~y:1 ~x:2 ~z:3

