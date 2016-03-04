(* Core / Persvasives *)
open Tinyocaml

(* This contains externals from Core / Pervasives *)
let builtin_output_string = function
  [OutChannel c; String s] -> output_string c s; Unit
| _ -> failwith "builtin_output_string"

let builtin_input_line = function
  [InChannel c] -> String (input_line c)
| _ -> failwith "builtin_input_line"

let mk f =
  Fun ("x", CallBuiltIn ([Var "x"], f))

let mk2 f =
  Fun ("x", Fun ("y", CallBuiltIn ([Var "x"; Var "y"], f)))

(* String to tinyocaml *)
let make_tiny s =
  s |> Lexing.from_string |> Parse.implementation |> Evalutils.getexpr |> of_real_ocaml

(* This contains pure ocaml functions for things in Core / Pervasives *)
let core =
  ["ref", make_tiny "fun x -> {contents = x}";
   "!", make_tiny "fun x -> x.contents";
   ":=", make_tiny "fun a -> fun b -> a.contents <- b";
   "output_string", mk2 builtin_output_string;
   "print_string", make_tiny "fun x -> output_string stdout x";
   "input_line", mk builtin_input_line]
