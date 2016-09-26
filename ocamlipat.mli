(* Read a search pattern using the OCaml lexer *)
val string_of_token : Parser.token -> string 

val tokens_of_lexbuf : Lexing.lexbuf -> Parser.token list

val regexp_of_lexbuf : Lexing.lexbuf -> Str.regexp

