open Evalutils

let quiet = ref false

let top = ref false

type mode =
  FromFile of string
| FromText of string

let source = ref None

let machine = ref "naive"

let printer = ref "tiny"

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
  s

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
    val to_string : t -> string
  end

let implementations =
  [("naive", (module Naive : Evaluator));
   ("naiveSimple", (module NaiveSimple : Evaluator));
   ("naiveSimpleOneStep", (module NaiveSimpleOneStep : Evaluator));
   ("environment", (module Environment : Evaluator))]

let argspec =
  [("-machine", Arg.Set_string machine, " Set the abstract machine");
   ("-quiet", Arg.Set quiet, " Print only the result");
   ("-pp", Arg.Set_string printer, " Set the prettyprinter");
   ("-e", Arg.String settext, " Evaluate the program text given");
   ("-top", Arg.Set top, " Do nothing, exit cleanly (for top level)")]

let load_code () =
  match !source with
    Some (FromFile s) -> Some (load_file s)
  | Some (FromText s) -> Some s
  | None -> None

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
        if not !quiet then begin
          if !printer = "tiny" then
            print_string (I.to_string state')
          else
            print_string (to_string (getexpr (I.tree state')));
          print_string "\n"
        end;
        really_run false state'
    | IsValue ->
        if !quiet then begin
          if !printer = "tiny" then
            print_string (I.to_string state)
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
    let state = I.init (ast code) in
      (* Print initial state, if not a value *)
      if not !quiet then
        begin
          if !printer = "tiny" then
            print_string (I.to_string state)
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

