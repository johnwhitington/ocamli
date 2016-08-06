type pat_elt =
    Any
  | Op of pat * string * pat
  | Text of string
  | Tuple of pat list
  | Or of pat * pat

and pat = pat_elt list

let parse_pattern p = Any



(* Use genlex to return a list of tokens, then convert it to our systsm *)
let lex s = []

let ex1 = parse_pattern (lex "_ + (0 | 1)")
let ex2 = parse_pattern (lex "sum _")
let ex3 = parse_pattern (lex "Br(_, Lf, Lf)")

(* For now, example patterns as things of type [pat] *)
let ex1 = [Op (Any, "+", Or (Text "0" | Text "1"))]
let ex2 = [Text "sum"; Any]
let ex3 = [Text "Br"; Tuple [Any; Text "Lf"; Text "Lf"]]

let program =
  Tinyocamlrw.of_real_ocaml [] (Ocamliutil.ast "1 + 2")

