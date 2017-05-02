open Ocamliutil
open Asttypes
open Parsetree

let show = ref false
let showall = ref false
let top = ref false
let debug = ref false
let showpervasives = ref false
let printer = ref "simple"
let width = ref 80
let show_simple_arithmetic = ref true
let debugtiny = ref false
let debugpp = ref false
let prompt = ref false
let step = ref 0.0
let fastcurry = ref false

type mode =
  FromFile of string
| FromText of string

let source = ref []

let modname_of_filename s =
  let stem = Bytes.of_string (Filename.remove_extension (Filename.basename s)) in
    if Bytes.length stem = 0 then "" else
      begin
        Bytes.set stem 0 (Char.uppercase_ascii (Bytes.get stem 0));
        Bytes.to_string stem
      end

let setfile s =
  source := (modname_of_filename s, FromFile s)::!source;
  Ocamliprim.exe := s

let settext ?(modname="") s =
  source := (modname, FromText s)::!source

let remove_recs = ref []

let add_remove_rec x =
  remove_recs := x::!remove_recs

let remove_rec_all = ref false

(* Load, appending, code from each source file or -e use. *)
let rec load_code = function
    (_, FromFile s)::r -> load_file s ^ " " ^ load_code r
  | (_, FromText s)::r -> s ^ " " ^ load_code r
  | [] -> ""

(* load_code should (A) Load all-but-the-last thing as modules, on top of the
 * stdlib (if present) and (B) Return the text of the top-level module,
 * disregarding its name. *)
let load_modules mods =
  Eval.lib :=
    Ocamlilib.load_library_modules
    (List.map (function (n, FromFile s) -> (n, load_file s) | (n, FromText s) -> (n, s)) mods)

let load_code () =
  match !source with
    [] -> None
  | h::t ->
      load_modules (List.rev t);
      Some (load_code [h])

let string_of_tiny ~preamble ?(codes=true) x =
  let x = Tinyocamlutil.remove_named_recursive_functions !remove_rec_all !remove_recs x in
    let temp = !Pptinyocaml.syntax in
      Pptinyocaml.syntax := codes && !Pptinyocaml.syntax;
      let r = Pptinyocaml.to_string ~preamble x in
        Pptinyocaml.syntax := temp;
        r

(* last: the op that got us here *)
(* next: the next op *)
(* prevstate: the previous state *)
(* currstate: the current state *)
let string_of_op = function
  Arith -> "Arith"
| Boolean -> "Boolean"
| Comparison -> "Comparison"
| IfBool -> "IfBool"
| InsidePervasive -> "InsidePervasive"

let show_this_stage last next prevstate currstate =
  (*Printf.printf "last = %s, next = %s\n" (string_of_op last) (string_of_op * next);*)
     Tinyocamlutil.is_value prevstate
  || Tinyocamlutil.is_value currstate
  || not (List.mem Arith last)
  || not (List.mem Arith next)

let show_this_pervasive_stage last =
  not (List.mem InsidePervasive last)

let skipped = ref false

let wait_for_enter () =
  ignore (input_line stdin)

let print_string x =
  print_string x;
  flush stdout

(* Evaluate a phrase to a string *)
let eval s =
  let state = Eval.init (Tinyocamlrw.of_real_ocaml [] (ast s)) in (* FIXME ENV *)
    let rec eval_inner state =
      match Eval.next state with
      | Next state -> eval_inner state
      | IsValue -> string_of_tiny ~preamble:"" (Eval.tiny state) 
      | Malformed s ->
          failwith "malformed"
      | Unimplemented s ->
          failwith "unimplemented"
    in
      eval_inner state

let extract_expression = function
  [{pstr_desc = Pstr_eval (e, _)}] -> e
| _ -> failwith "extract_expression"

(* Structure to structure/expression *)
let eval_ast structure =
  let state = Eval.init structure in
    let rec eval_inner state =
      match Eval.next state with
      | Next state ->
          eval_inner state
      | IsValue ->
          extract_expression (Tinyocamlrw.to_real_ocaml (Eval.tiny state))
      | Malformed _ | Unimplemented _ ->
          failwith "evaluation failed"
    in
      eval_inner state

let extract_tiny = function
  Tinyocaml.Struct (_, [x]) -> x
| _ -> failwith "extract_tiny"

(* String to Tinyocaml.t result *)
let eval_string s =
  let state = Eval.init (Tinyocamlrw.of_real_ocaml [] (ast s)) in (* FIXME ENV *)
    let rec eval_inner state =
      match Eval.next state with
      | Next state ->
          eval_inner state
      | IsValue ->
          extract_tiny (Eval.tiny state)
      | Malformed _ | Unimplemented _ ->
          failwith "evaluation failed"
    in
      eval_inner state
    
let eval_string_to_ast s =
  let state = Eval.init (Tinyocamlrw.of_real_ocaml [] (ast s)) in (* FIXME ENV *)
    let rec eval_inner state =
      match Eval.next state with
      | Next state ->
          eval_inner state
      | IsValue ->
          extract_expression (Tinyocamlrw.to_real_ocaml (Eval.tiny state))
      | Malformed _ | Unimplemented _ ->
          failwith "evaluation failed"
    in
      eval_inner state
