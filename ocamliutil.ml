(* For writing evaluators with *)
open Parsetree
open Asttypes

(* Return the set of distinct  elements in a list. Does not preserve order. *)
let setify_simple l =
  let rec setify_inner r = function
    | [] -> r
    | h::t ->
        if List.mem h t
          then setify_inner r t
          else setify_inner (h::r) t
  in
    setify_inner [] l

let setify = setify_simple

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

let modname_of_filename s =
  let stem = Bytes.of_string (Filename.remove_extension (Filename.basename s)) in
    if Bytes.length stem = 0 then "" else
      begin
        Bytes.set stem 0 (Char.uppercase_ascii (Bytes.get stem 0));
        Bytes.to_string stem
      end

let filename_of_modname = function
  "" -> ""
| x ->
    let chars = explode x in
      implode (Char.lowercase_ascii (List.hd chars)::(List.tl chars)) ^ ".ml"

let typecheck = ref true

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let env =
  Compmisc.init_path false;
  Compmisc.initial_env ()

let ast ?(filename="") code =
  let ast =
    let lexer = Lexing.from_string code in
    Location.init lexer filename;
    Parse.implementation lexer
  in
    if not !typecheck then ast else
      try
        let _, _ = Typemod.type_implementation "foo.ml" "" "example" env ast in
        ast
      with
        e ->
          Location.report_exception Format.std_formatter e;
          exit 2

type 'a result =
    Next of 'a
  | IsValue
  | Malformed of string
  | Unimplemented of string

exception ExnUnimplemented of string
exception ExnMalformed of string

type last_op =
    Arith
  | Boolean
  | Comparison
  | IfBool
  | InsidePervasive
  | VarLookup

let unimp s = raise (ExnUnimplemented s)
let malformed s = raise (ExnMalformed s)

let ast_to_string ast =
  let b = Buffer.create 80 in
  let formatter = Format.formatter_of_buffer b in
    Pprintast.expression formatter ast;
    Format.pp_print_flush formatter ();
    Buffer.contents b

let with_desc x =
  {pexp_desc = x;
   pexp_loc = Location.none;
   pexp_attributes = []}

let getexpr = function
  [{pstr_desc = Pstr_eval (e, _)}]
| [{pstr_desc =
     Pstr_value
       (Nonrecursive, [{pvb_pat = {ppat_desc = Ppat_any}; pvb_expr = e}])}] -> e
| _ -> failwith "Not a single structure item"

let isstarred s =
  String.length s > 1 && (String.sub s 0 1 = "*" || String.sub s 0 1 = "[")

let star s = "*" ^ s

let unstar s =
  if isstarred s then String.sub s 1 (String.length s - 1) else s

let rec option_map f = function
  | [] -> []
  | h::t ->
      match f h with
        None -> option_map f t
      | Some x -> x::option_map f t


