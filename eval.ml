open Evalutils

let quiet = ref false

let top = ref false

type mode =
  FromFile of string
| FromText of string

let source = ref None

let machine = ref "environment"

let printer = ref "tiny"

let show_simple_arithmetic = ref true

let setfile s =
  source := Some (FromFile s)

let settext s =
  source := Some (FromText s)

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

(* Main loop. Get the right abstract machine module, and keep calling it until
IsValue or Malformed or Unimplemented *)
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
    val last : unit -> Evalutils.last_op
    val peek : t -> Evalutils.last_op
  end

let implementations =
  [("naive", (module Naive : Evaluator));
   ("naiveSimple", (module NaiveSimple : Evaluator));
   ("naiveSimpleOneStep", (module NaiveSimpleOneStep : Evaluator));
   ("environment", (module Environment : Evaluator))]

let remove_recs = ref []

let add_remove_rec x =
  remove_recs := x::!remove_recs

let argspec =
  [("-machine", Arg.Set_string machine, " Set the abstract machine");
   ("-quiet", Arg.Set quiet, " Print only the result");
   ("-pp", Arg.Set_string printer, " Set the prettyprinter");
   ("-e", Arg.String settext, " Evaluate the program text given");
   ("-top", Arg.Set top, " Do nothing, exit cleanly (for top level)");
   ("-remove-rec", Arg.String add_remove_rec, " No not print the given recursive function");
   ("-no-arith", Arg.Clear show_simple_arithmetic, " Ellide simple arithmetic")]

let load_code () =
  match !source with
    Some (FromFile s) -> Some (load_file s)
  | Some (FromText s) -> Some s
  | None -> None

let string_of_tiny x =
  let x = TinyocamlUtils.remove_named_recursive_functions !remove_recs x in
    Pptinyocaml.to_string x

(* FIXME: For now, just changes nothing. It's hard to underline the whole set
 * of redexes. Maybe we can push -no-arith into environment.ml rather than
 * patching in eval.ml? *)
let underline_whole_first_arithmetic x = x
  (*TinyocamlUtils.strip_control x*)
  (*Tinyocaml.Control (Tinyocaml.Underline, Tinyocaml.strip_control x)*)

let fixup op x =
  if op = Arith && not !show_simple_arithmetic
    then underline_whole_first_arithmetic x
    else x

(* last: the op that got us here *)
(* next: the next op *)
(* prevstate: the previous state *)
(* currstate: the current state *)
let string_of_op = function
  Arith -> "Arith"
| Unknown -> "Unknown"
| x -> "Other"

let show_this_stage last next prevstate currstate =
  (*Printf.printf "last = %s, next = %s\n" (string_of_op last) (string_of_op * next);*)
     TinyocamlUtils.is_value prevstate
  || TinyocamlUtils.is_value currstate
  || last <> Arith
  || next <> Arith

let skipped = ref false

let () =
  Arg.parse argspec setfile
    "Syntax: eval <filename | -e program>
             [-quiet]
             [-pp <ocaml | tiny>]
             [-machine <naive | naiveSimple | naiveSimpleOneStep | cc | scc | ck | cek | secd>]\n";
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
        (string_of_tiny (I.tiny state')) !skipped;*)
        begin if
          not !quiet && (!show_simple_arithmetic ||
            show_this_stage
              (I.last ()) (I.peek state') (I.tiny state) (I.tiny state'))
        then
          begin
            if !printer = "tiny" then
              begin
                print_string (if !skipped then "=>* " else "=>  ");
                print_string (string_of_tiny (fixup (I.peek state') (I.tiny state')))
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
        if !quiet then begin
          if !printer = "tiny" then
            begin
              print_string (if first then "    " else if !skipped then "=>* " else "=>  ");
              print_string (string_of_tiny (fixup (I.peek state) (I.tiny state)))
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
    let state = I.init (ast code) in
      (* Print initial state, if not a value *)
      if not !quiet then
        begin
          if !printer = "tiny" then
            begin
              print_string "    ";
              print_string (string_of_tiny (fixup (I.peek state) (I.tiny state)))
            end
          else
            print_string (to_string (getexpr (I.tree state)));
          print_string "\n"
        end;
      really_run true state
   in
      try
        if not !top then
          match load_code () with
            None -> failwith "No source code provided"
          | Some x -> run x
      with
        e -> Printf.eprintf "Error: [%s]\n" (Printexc.to_string e); exit 1

