let parse_pattern p =

type pat =
    Any
  | Op of pat * string * pat
  | Text of string
  | Tuple of pat list

(* Use genlex to return a list of tokens, then convert it to our systsm *)
let lex = 

let ex1 = parse_pattern (lex "_ + _")
let ex2 = parse_pattern (lex "sum _")
let ex3 = parse_pattern (lex "Br(_, Lf, Lf)")

let program =
  Tinyocamlrw.of_real_ocaml [] (Ocamliutil.ast "1 + 2")

