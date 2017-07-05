let p1 = (function "A" -> 0) "A"

let p2 = (function 'A' -> 0) 'A'

let p3 = (function 'A'..'Z' -> 0) 'P'

let p4 = (function 1L -> 0) 1L

let p5 = (function 1l -> 0) 1l

let p6 = (function 1n -> 0) 1n

let p7 = (function 1 | 2 -> 0) 2

