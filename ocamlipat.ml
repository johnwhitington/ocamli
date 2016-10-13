open Parser

let special_case = function
  "_" -> ".*"
| x -> Str.quote x

(* Read a search pattern using the OCaml lexer *)
let string_of_token = function
  | AMPERAMPER -> "&&"
  | AMPERSAND -> "&"
  | AND -> "and"
  | AS -> "as"
  | ASSERT -> "assert"
  | BACKQUOTE -> "`"
  | BANG -> "!"
  | BAR -> "|"
  | BARBAR -> "||"
  | BARRBRACKET -> "|]" 
  | BEGIN -> "begin"
  | CHAR c -> Printf.sprintf "%C" c
  | CLASS -> "class"
  | COLON -> ":"
  | COLONCOLON -> "::"
  | COLONEQUAL -> ":="
  | COLONGREATER -> ":>"
  | COMMA -> ","
  | CONSTRAINT -> "constraint" 
  | DO -> "do"
  | DONE -> "done"
  | DOT -> "."
  | DOTDOT -> ".."
  | DOWNTO -> "downto"
  | ELSE -> "else"
  | END -> "end"
  | EOF -> ""
  | EQUAL -> "="
  | EXCEPTION -> "exception"
  | EXTERNAL -> "external"
  | FALSE -> "false"
  | FLOAT (s, _) -> Printf.sprintf "%s" s (*FIXME: Check this. In general, floats will be a bit of a problem... *)
  | FOR -> "for"
  | FUN -> "fun"
  | FUNCTION -> "function"
  | FUNCTOR -> "functor"
  | GREATER -> ">"
  | GREATERRBRACE -> ">}"
  | GREATERRBRACKET -> ">]"
  | IF -> "if"
  | IN -> "in"
  | INCLUDE -> "include"
  | INFIXOP0 s -> s
  | INFIXOP1 s -> s
  | INFIXOP2 s -> s
  | INFIXOP3 s -> s
  | INFIXOP4 s -> s
  | INHERIT -> "inherit"
  | INITIALIZER -> "initializer"
  | INT (s, _) -> Printf.sprintf "%s" s (* FIXME: modifiers *)
  | LABEL s -> s (* FIXME: Check *) 
  | LAZY -> "lazy"
  | LBRACE -> "{"
  | LBRACELESS -> "{<"
  | LBRACKET -> "["
  | LBRACKETBAR -> "[|"
  | LBRACKETLESS -> "[<"
  | LBRACKETGREATER -> "[>"
  | LBRACKETPERCENT -> "[%"
  | LBRACKETPERCENTPERCENT -> "[%%"
  | LESS -> "<"
  | LESSMINUS -> "<-"
  | LET -> "let"
  | LIDENT s -> s
  | LPAREN -> "("
  | LBRACKETAT -> "[@"
  | LBRACKETATAT -> "[@@"
  | LBRACKETATATAT -> "[@@@"
  | MATCH -> "match"
  | METHOD -> "method"
  | MINUS -> "-"
  | MINUSDOT -> "-."
  | MINUSGREATER -> "->"
  | MODULE -> "module"
  | MUTABLE -> "mutable"
  | NEW -> "new"
  | NONREC -> "nonrec"
  | OBJECT -> "object"
  | OF -> "of"
  | OPEN -> "open"
  | OPTLABEL s -> s (* FIXME: check *)
  | OR -> "or"
  | PERCENT -> "%"
  | PLUS -> "+"
  | PLUSDOT -> "+."
  | PLUSEQ -> "+="
  | PREFIXOP s -> s
  | PRIVATE -> "private"
  | QUESTION -> "?"
  | QUOTE -> "\""
  | RBRACE -> "}"
  | RBRACKET -> "]"
  | REC -> "rec"
  | RPAREN -> ")"
  | SEMI -> ";"
  | SEMISEMI -> ";;"
  | SHARP -> "#"
  | SHARPOP s -> s
  | SIG -> "sig"
  | STAR -> "*"
  | STRING (s, _) -> s
  | STRUCT -> "struct"
  | THEN -> "then"
  | TILDE -> "~"
  | TO -> "to"
  | TRUE -> "true"
  | TRY -> "try"
  | TYPE -> "type"
  | UIDENT s -> s
  | UNDERSCORE -> "_"
  | VAL -> "val"
  | VIRTUAL -> "virtual"
  | WHEN -> "when"
  | WHILE -> "while"
  | WITH -> "with"
  | COMMENT (s, _) -> s
  | DOCSTRING _ -> "FIXMEDOCSTRING"
  | EOL -> ""

let tokens_of_lexbuf l =
  let toks = ref [] in
    try
      while true do
        let tok = Lexer.token l in
          if tok = EOF then raise Exit else toks := tok :: !toks;
      done;
      []
    with
      Exit -> List.rev !toks

let whitespace_of_items items =
  "\\(" ^ List.fold_left ( ^ ) "" (List.map (fun x -> x ^ "\\|") items) ^ "\\)*"

let classic_whitespace_items = [" "; "\\t"; "\\n"; "\\f"; "\\t"]

let paren_items = ["("; ")"; "begin"; "end"]

let whitespace =
  whitespace_of_items
    ((*(if !noparens then paren_items else []) @*) classic_whitespace_items)

let regexp_of_lexbuf l =
  let terms =
    let tokens = List.map string_of_token (tokens_of_lexbuf l) in
      List.map special_case tokens
  in
    String.concat whitespace terms

let regexp_of_string s =
  regexp_of_lexbuf (Lexing.from_string s)

