(* Core / Persvasives *)
open Tinyocaml

(* This contains externals from Core / Pervasives *)
let builtin_output_string = function
  [OutChannel c; String s] -> output_string c s; Unit
| _ -> failwith "builtin_output_string"

let builtin_print_int = function
  [Int i] -> output_string stdout (string_of_int i); Unit
| _ -> failwith "builtin_print_int"

let builtin_input_line = function
  [InChannel c] -> String (input_line c)
| _ -> failwith "builtin_input_line"

let mk name f =
  Fun {fname = "x"; fexp = CallBuiltIn (name, [Var "x"], f); fper = true}

let mk2 name f =
  Fun {fname = "x";
       fexp = Fun {fname = "y"; fexp = CallBuiltIn (name, [Var "x"; Var "y"], f); fper = true};
       fper = true}

(* String to tinyocaml *)
let make_tiny s =
  match
    s |> Lexing.from_string |> Parse.implementation |> of_real_ocaml ~allpervasive:true
  with
    Module [h] -> h
  | _ -> failwith "make_tiny"

(* This contains pure ocaml functions for things in Core / Pervasives *)
let core =
  ["ref", make_tiny "fun x -> {contents = x}";
   "!", make_tiny "fun x -> x.contents";
   ":=", make_tiny "fun a -> fun b -> a.contents <- b";
   "output_string", mk2 "output_string" builtin_output_string;
   "print_string", make_tiny "fun x -> output_string stdout x";
   "print_int", mk "print_int" builtin_print_int;
   "input_line", mk "input_line" builtin_input_line]
