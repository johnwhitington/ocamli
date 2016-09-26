(* File calc.ml *)
let string_of_token = function
  | Parser.INT i -> Printf.sprintf "INT %i" i
  | Parser.LPAREN -> Printf.sprintf "LPAREN"
  | Parser.RPAREN -> Printf.sprintf "RPAREN"
  | Parser.EOL -> Printf.sprintf "EOL"

let _ =
  let lexbuf = Lexing.from_channel stdin in
  try
    while true do
      let result = Lexer.token lexbuf in
        if result = Parser.EOL then raise Exit;
        print_string (string_of_token result);
        print_newline ()
    done
  with
    Exit -> ()

