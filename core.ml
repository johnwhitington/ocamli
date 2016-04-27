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

let builtin_int_of_string = function
  [String s] ->
    begin try Int (int_of_string s) with
      e -> failwith "builtin_int_of_string" (* FIXME: A Proper exception here *)
    end
| _ -> failwith "builtin_int_of_string"

(* The initial asterisk will be used to elide these variables when not showing
pervasives in the output. When showing pervasives, we just remove the asterisk. *)
let mk name f =
  Fun ("__PER__x", CallBuiltIn (name, [Var "__PER__x"], f))

let mk2 name f =
  Fun ("__PER__x", Fun ("__PER__y", CallBuiltIn (name, [Var "__PER__x"; Var "__PER__y"], f)))

(* String to tinyocaml *)
let make_tiny s =
  match
    s |> Lexing.from_string |> Parse.implementation |> of_real_ocaml
  with
    Module [h] -> h
  | _ -> failwith "make_tiny"

(* This contains pure ocaml functions for things in Core / Pervasives *)
let core =
  ["ref", make_tiny "fun __PER__x -> {contents = __PER__x}";
   "!", make_tiny "fun __PER__x -> __PER__x.contents";
   ":=", make_tiny "fun __PER__a -> fun __PER__b -> __PER__a.contents <- __PER__b";
   "output_string", mk2 "output_string" builtin_output_string;
   "print_string", make_tiny "fun __PER__x -> output_string stdout __PER__x";
   "print_int", mk "print_int" builtin_print_int;
   "int_of_string", mk "int_of_string" builtin_int_of_string;
   "input_line", mk "input_line" builtin_input_line]
