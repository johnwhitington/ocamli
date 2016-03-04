(* Core / Persvasives *)
open Tinyocaml

(* This contains externals from Core / Pervasives *)
let builtin_output_string = function
  [OutChannel c; String s] -> output_string c s; Unit
| _ -> failwith "builtin_output_string"

let builtins =
  ["output_string",
      Fun ("c", Fun ("s", CallBuiltIn ([Var "c"; Var "s"], builtin_output_string)))]

(* Lookup a built-in function by its name *)
let lookup_builtin x =
  List.assoc x builtins

(* String to tinyocaml *)
let make_tiny s =
  s |> Lexing.from_string |> Parse.implementation |> Evalutils.getexpr |> of_real_ocaml

(* This contains pure ocaml functions for things in Core / Pervasives *)
let core =
  ["ref", make_tiny "fun x -> {contents = x}";
   "!", make_tiny "fun x -> x.contents";
   ":=", make_tiny "fun a -> fun b -> a.contents <- b";
   "print_string", make_tiny "fun x -> output_string stdout x"]
