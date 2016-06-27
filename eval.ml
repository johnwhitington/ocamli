open Evalutils
open Asttypes
open Parsetree

let silent = ref false
let quiet = ref false
let top = ref false
let debug = ref false
let showpervasives = ref false
let machine = ref "environment"
let printer = ref "tiny"
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

let source = ref None

let setfile s =
  source := Some (FromFile s)

let settext s =
  source := Some (FromText s)

module type Evaluator =
  sig
    type t
    val init : Parsetree.structure -> t
    val next : t -> t Evalutils.result
    val tiny : t -> Tinyocaml.t
    val to_string : t -> string
    val last : unit -> Evalutils.last_op list
    val peek : t -> Evalutils.last_op list
    val newlines : t -> bool
    val fastcurry : bool ref
  end

let implementations =
  [(*("naive", (module Naive : Evaluator));
   ("naiveSimple", (module NaiveSimple : Evaluator));
   ("naiveSimpleOneStep", (module NaiveSimpleOneStep : Evaluator));*)
   ("environment", (module Environment : Evaluator))]

let remove_recs = ref []

let add_remove_rec x =
  remove_recs := x::!remove_recs

let remove_rec_all = ref false

let load_code () =
  match !source with
    Some (FromFile s) -> Some (load_file s)
  | Some (FromText s) -> Some s
  | None -> None

let string_of_tiny ~preamble x =
  let x = TinyocamlUtils.remove_named_recursive_functions !remove_rec_all !remove_recs x in
    Pptinyocaml.to_string ~preamble x

let fixup ops x = x

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
     TinyocamlUtils.is_value prevstate
  || TinyocamlUtils.is_value currstate
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
  let state = Environment.init (ast s) in
    let rec eval_inner state =
      match Environment.next state with
      | Next state -> eval_inner state
      | IsValue -> string_of_tiny ~preamble:"" (Environment.tiny state) 
      | Malformed s ->
          failwith "malformed"
      | Unimplemented s ->
          failwith "unimplemented"
    in
      eval_inner state

let extract_expression [{pstr_desc = Pstr_eval (e, _)}] = e

(* Structure to structure/expression *)
let eval_ast structure =
  let state = Environment.init structure in
    let rec eval_inner state =
      match Environment.next state with
      | Next state ->
          eval_inner state
      | IsValue ->
          extract_expression (Tinyocaml.to_real_ocaml (Environment.tiny state))
      | Malformed _ | Unimplemented _ ->
          failwith "evaluation failed"
    in
      eval_inner state

let extract_tiny = function
  Tinyocaml.Struct [x] -> x

(* String to Tinyocaml.t result *)
let eval_string s =
  let state = Environment.init (ast s) in
    let rec eval_inner state =
      match Environment.next state with
      | Next state ->
          eval_inner state
      | IsValue ->
          extract_tiny (Environment.tiny state)
      | Malformed _ | Unimplemented _ ->
          failwith "evaluation failed"
    in
      eval_inner state
    
let eval_string_to_ast s =
  let state = Environment.init (ast s) in
    let rec eval_inner state =
      match Environment.next state with
      | Next state ->
          eval_inner state
      | IsValue ->
          extract_expression (Tinyocaml.to_real_ocaml (Environment.tiny state))
      | Malformed _ | Unimplemented _ ->
          failwith "evaluation failed"
    in
      eval_inner state
