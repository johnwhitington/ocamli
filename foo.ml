(*let rec erase_rel : type a b c d e f g h i j k l .
  (a, b, c, d, e, f,
   g, h, i, j, k, l) fmtty_rel -> (a, b, c, d, e, f) fmtty
= function
  | Char_ty rest ->
    Char_ty (erase_rel rest)*)

let rec f : type a . a = 0
