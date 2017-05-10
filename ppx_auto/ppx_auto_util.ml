open Parsetree

(* Make a list of characters from a string, preserving order. *)
let explode s =
  let l = ref [] in
    for p = String.length s downto 1 do
      l := String.unsafe_get s (p - 1)::!l
    done;
    !l

(* Make a string from a list of characters, preserving order. *)
let implode l =
  let s = Bytes.create (List.length l) in
    let rec list_loop x = function
       [] -> ()
     | i::t -> Bytes.unsafe_set s x i; list_loop (x + 1) t
    in
      list_loop 0 l;
      Bytes.to_string s

let string_of_char c =
  let s = String.create 1 in
    String.unsafe_set s 0 c;
    s

let getexpr = function
  [{pstr_desc = Pstr_eval (e, _)}] -> e 
| _ -> failwith "getexpr: Not a single structure item"

let ast s =
  Parse.implementation (Lexing.from_string s)

