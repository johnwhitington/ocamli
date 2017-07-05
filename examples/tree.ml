type 'a tree = Lf | Br of 'a tree * 'a * 'a tree

let t = Br (Lf, 3, Br (Lf, 4, Lf))

let rec sum = function
  Lf -> 0 | Br (l, x, r) -> sum l + x + sum r

let x = sum t

