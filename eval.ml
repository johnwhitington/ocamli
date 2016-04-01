open Evalutils

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

type mode =
  FromFile of string
| FromText of string

let source = ref None

let setfile s =
  source := Some (FromFile s)

let settext s =
  source := Some (FromText s)

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let ast code =
  code |> Lexing.from_string |> Parse.implementation

module type Evaluator =
  sig
    type t
    val init : Parsetree.structure -> t
    val next : t -> t Evalutils.result
    val tree : t -> Parsetree.structure
    val tiny : t -> Tinyocaml.t
    val to_string : t -> string
    val last : unit -> Evalutils.last_op list
    val peek : t -> Evalutils.last_op list
    val newlines : t -> bool
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

let argspec =
  [("-machine", Arg.Set_string machine, " Set the abstract machine");
   ("-quiet", Arg.Set quiet, " Print only the result");
   ("-silent", Arg.Set silent, " Print only what the program prints");
   ("-pp", Arg.Set_string printer, " Set the prettyprinter");
   ("-width", Arg.Set_int width, " Set the output width");
   ("-e", Arg.String settext, " Evaluate the program text given");
   ("-top", Arg.Set top, " Do nothing, exit cleanly (for top level)");
   ("-remove-rec", Arg.String add_remove_rec, " Do not print the given recursive function");
   ("-remove-rec-all", Arg.Set remove_rec_all, " Do not print any recursive functions");
   ("-show-pervasives", Arg.Set showpervasives, " Show Pervasives such as :=");
   ("-dtiny", Arg.Set debugtiny, " Show Tinyocaml representation");
   ("-dpp", Arg.Set debugpp, " Show the pretty-printed program");
   ("-debug", Arg.Set debug, " Debug (for OCAMLRUNPARAM=b)");
   ("-no-arith", Arg.Clear show_simple_arithmetic, " Ellide simple arithmetic")]

let load_code () =
  match !source with
    Some (FromFile s) -> Some (load_file s)
  | Some (FromText s) -> Some s
  | None -> None

let string_of_tiny ~preamble x =
  let x = TinyocamlUtils.remove_named_recursive_functions !remove_rec_all !remove_recs x in
    Pptinyocaml.to_string ~preamble x

(* FIXME: For now, just changes nothing. It's hard to underline the whole set
 * of redexes. Maybe we can push -no-arith into environment.ml rather than
 * patching in eval.ml? *)
let underline_whole_first_arithmetic x = x
  (*TinyocamlUtils.strip_control x*)
  (*Tinyocaml.Control (Tinyocaml.Underline, Tinyocaml.strip_control x)*)

let fixup ops x = x
  (*if op = Arith && not !show_simple_arithmetic
    then underline_whole_first_arithmetic x
    else x*)

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

let () =
  Arg.parse argspec setfile
    "Syntax: eval <filename | -e program>
             [-pp <ocaml | tiny* | simple ]
             [-machine <naive | naiveSimple | naiveSimpleOneStep | environment*]\n";
  let module I =
    (val
       (try List.assoc !machine implementations with
         _ -> failwith "Unknown machine"
       ) : Evaluator)
  in
  let rec really_run first state =
    match I.next state with
      Next state' ->
        (*Printf.printf "Considering printing stage %s...skipped last is %b\n"
        (string_of_tiny ~preamble:"" (I.tiny state')) !skipped;*)
        begin if
          not (!quiet || !silent) &&
          (!show_simple_arithmetic || show_this_stage (I.last ()) (I.peek state') (I.tiny state) (I.tiny state')) &&
          (!showpervasives || show_this_pervasive_stage (I.last ()))
        then
          begin
            if !printer = "tiny" then
              begin
                let preamble = if !skipped then "=>* " else "=>  " in
                if I.newlines state then print_string "\n";
                print_string (string_of_tiny ~preamble (fixup (I.peek state') (I.tiny state')))
              end
            else
              print_string (to_string (getexpr (I.tree state')));
            skipped := false;
            print_string "\n"
          end
        else
          skipped := true
        end;
        really_run false state'
    | IsValue ->
        (* Only print if !quiet. On Silent we don't want it, on normal, we have already printed *)
        if !quiet then begin
          if !printer = "tiny" then
            begin
              print_string (string_of_tiny ~preamble:"=>* " (fixup (I.peek state) (I.tiny state)))
            end
          else
            print_string (to_string (getexpr (I.tree state)));
          print_string "\n"
        end
    | Malformed s ->
        print_string "Malformed AST node\n";
        print_string s;
        print_string "\n"
    | Unimplemented s ->
        print_string "Unimplemented AST node\n";
        print_string s;
        print_string "\n"
  in
   let run code =
    if !printer = "simple" then
      begin
        Pptinyocaml.simple := true;
        printer := "tiny"
      end;
    Pptinyocaml.width := !width;
    let state = I.init (ast code) in
       if !debugtiny then
         begin
           print_string (Tinyocaml.to_string (I.tiny state));
           print_string "\n";
           flush stdout;
           exit 0
         end;
      if not (!quiet || !silent) then
        begin
          if !printer = "tiny" then
            print_string (string_of_tiny ~preamble:"    " (fixup (I.peek state) (I.tiny state)))
          else
            print_string (to_string (getexpr (I.tree state)));
          print_string "\n"
        end;
      if !debugpp then exit 0;
      really_run true state
   in
     try
       if not !top then
         match load_code () with
           None -> failwith "No source code provided"
         | Some x -> run x
     with
       e ->
         if !debug then raise e else Printf.eprintf "Error: [%s]\n" (Printexc.to_string e);
         exit 1

