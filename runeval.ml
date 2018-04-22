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
let noifbool = ref false
let novarlookup = ref false

type mode =
  FromFile of string
| FromText of string

let source = ref []


let setfile s =
  source := (modname_of_filename s, FromFile s)::!source;
  Ocamliprim.exe := (Bytes.of_string s)

let settext ?(modname="") s =
  source := (modname, FromText s)::!source

let remove_recs = ref []

let add_remove_rec x =
  remove_recs := x::!remove_recs

let remove_rec_all = ref false

(* Load, appending, code from each source file or -e use. *)
let rec load_code = function
    (_, FromFile s) -> load_file s
  | (_, FromText s) -> s

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
      Some (load_code h)

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
| VarLookup -> "VarLookup"

let string_of_ops ops =
  List.fold_left (fun a b -> a ^ " " ^ b) "" (List.map string_of_op ops)

let debug_last_next last next =
  Printf.printf "LAST: %s\n" (string_of_ops last);
  Printf.printf "NEXT: %s\n" (string_of_ops next)

let show_this_stage last next prevstate currstate =
  (*Printf.printf "noifbool = %b\n" !noifbool;
  debug_last_next last next;*)
  if !noifbool && List.mem IfBool next then false else
  if !novarlookup && List.mem VarLookup last then false else
  let r =
     Tinyocamlutil.is_value prevstate
  || Tinyocamlutil.is_value currstate
  || not (List.mem Arith last)
  || not (List.mem Arith next)
  in
    (*Printf.printf "show_this_stage: %b\n" r;*)
    r

let show_this_pervasive_stage last =
  not (List.mem InsidePervasive last)

let skipped = ref false

let call_editor (current_state : Eval.t) =
  let editor =
    match Sys.getenv_opt "EDITOR" with
      Some x -> x
    | None -> "vim"
  in
    let fname = Filename.temp_file "ocamli" "editcall" in
    let temp = open_out fname in
    output_string temp (Pptinyocaml.to_string current_state);
    close_out temp;
    let newtext =
      match Sys.command (editor ^ " " ^ fname) with
        0 -> load_file fname
      | _ -> failwith "calling editor failed"
    in
      Sys.remove fname;
      newtext

let rec wait_for_enter (current_state : Eval.t) =
  Printf.printf "\n?%!";
  match input_line stdin with
    "" -> None
  | "edit" -> Some (call_editor current_state)
  | "back" ->
      begin match Eval.pop_state () with
        None -> None
      | Some x -> Some x
      end
  | _ -> Printf.printf "unknown command\n%!"; wait_for_enter current_state

let print_string x =
  print_string x;
  flush stdout

let extract_expression = function
  [{pstr_desc = Pstr_eval (e, _)}] -> e
| _ -> failwith "extract_expression"

let extract_tiny = function
  Tinyocaml.Struct (_, [x]) -> x
| _ -> failwith "extract_tiny"

(* This is just for the use of ppx_eval *)
let eval_string_to_ast ?(stdlib=true) ?(filename="") s =
  if stdlib then Ocamlilib.load_library ();
  let state = Eval.init (snd (Tinyocamlrw.of_real_ocaml [] (ast ~filename s))) in (* FIXME ENV *)
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

