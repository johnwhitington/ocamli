(* For writing evaluators with *)
open Parsetree
open Asttypes
open Tinyocaml

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
  String.length s > 1 && String.sub s 0 1 = "*"

let star s = "*" ^ s

let unstar s =
  if String.length s > 1 && String.sub s 0 1 = "*"
    then String.sub s 1 (String.length s - 1)
    else s

let rec option_map f = function
  | [] -> []
  | h::t ->
      match f h with
        None -> option_map f t
      | Some x -> x::option_map f t

(* Opening a module Find any items in the environment beginning with 'n', strip the name, and
duplicate them at the top of the environment. *)
let begins_with n s =
  String.length n <= String.length s &&
  n = String.sub s 0 (String.length n)

let rec pattern_begins_with n = function
  PatVar s when begins_with n s -> true
| PatTuple ts when List.for_all (pattern_begins_with n) ts -> true
| _ -> false

let binding_begins_with n (p, e) =
  pattern_begins_with n p

let bindings_beginning_with n env =
  option_map
    (function envitem ->
      match envitem with
        EnvFunctor (func_name, input_module_name, modtype, e, env) ->
          if begins_with n func_name
            then Some envitem
            else None
      | EnvBinding (recflag, bindings) ->
          if List.for_all (binding_begins_with n) !bindings
            then Some envitem
            else None)
    env

let cut n s =
  String.sub s (String.length n + 1) (String.length s - String.length n - 1)

let rec strip_pattern n = function
  PatVar s -> PatVar (cut n s)
| PatTuple ts -> PatTuple (List.map (strip_pattern n) ts)
| _ -> failwith "implement Ocamliutil.strip_pattern"

let strip_binding n (p, e) = (strip_pattern n p, e)

let strip_bindings n = function
  EnvFunctor (s, input_module_name, modtype, e, env) ->
    EnvFunctor (cut n s, input_module_name, modtype, e, env)
| EnvBinding (recflag, bs) ->
    EnvBinding (recflag, ref (List.map (strip_binding n) !bs))

let open_module n (env : Tinyocaml.env) =
  List.map (strip_bindings n) (bindings_beginning_with n env) @ env

let rec prefix_pattern prefix = function
  PatVar s -> PatVar (prefix ^ "." ^ s)
| PatTuple ts -> PatTuple (List.map (prefix_pattern prefix) ts)
| _ -> failwith "implement Ocamliutil.prefix_pattern"

let prefix_binding prefix (p, e) = (prefix_pattern prefix p, e)

let prefix_bindings p = function
  EnvBinding (recflag, bs) ->
    EnvBinding (recflag, ref (List.map (prefix_binding p) !bs))
| EnvFunctor (n, input_module_name, modtype, e, env) ->
    EnvFunctor (p ^ n, input_module_name, modtype, e, env)

(* For "module B = Bytes" Find any binding beginning with 'Bytes', replace
'Bytes' with 'B', and stick on to the front of the environment. *)
let alias_module current alias (env : Tinyocaml.env) =
  (*Printf.printf "Aliasing %s --> %s\n" current alias;*)
  let replaced =
    List.map
      (prefix_bindings alias)
      (List.map
        (strip_bindings current)
        (bindings_beginning_with current env))
  in
    replaced @ env

(* Assuming all the bindings are values already, add them to the environment as
Name.x, Name.y etc. *)
let bindings_of_struct_item p = function
  | LetDef (b, ld) -> Some (prefix_bindings p (EnvBinding (b, ref ld)))
  (*FIXME Add creation of EnvFunctor here, from a functor found in the struct. *)
  | _ -> None

let open_struct_as_module name items (env : Tinyocaml.env) =
  let bindings = option_map (bindings_of_struct_item name) items in
    let top_level_binding =
      EnvBinding (false, ref [(PatVar name, Struct (false, items))])
    in
      top_level_binding :: bindings @ env

